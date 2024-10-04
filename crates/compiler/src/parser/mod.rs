mod scan;
mod token;

use std::borrow::Cow;
use std::u32;

use rts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;
use token::{BinPrec, Token, TokenKind};

use crate::ast::{self, BinOp, Node, NodeID};
use crate::atoms::{AtomId, AtomMap};
use crate::keyword::{self, IDENTIFIER, KEYWORDS};

type PResult<T> = Result<T, ()>;

pub struct NodeMap<'cx>(FxHashMap<NodeID, Node<'cx>>);

impl<'cx> NodeMap<'cx> {
    pub fn get(&self, id: NodeID) -> Node<'cx> {
        self.0.get(&id).copied().unwrap()
    }

    pub fn insert(&mut self, id: NodeID, node: Node<'cx>) {
        let prev = self.0.insert(id, node);
        assert!(prev.is_none())
    }
}

#[derive(Debug)]
pub struct ParentMap(FxHashMap<NodeID, NodeID>);

impl ParentMap {
    pub fn parent(&self, id: NodeID) -> Option<NodeID> {
        self.0.get(&id).copied()
    }

    pub fn insert(&mut self, id: NodeID, parent: NodeID) {
        let prev = self.0.insert(id, parent);
        assert!(prev.is_none())
    }

    pub fn r#override(&mut self, id: NodeID, parent: NodeID) {
        let prev = self.0.insert(id, parent);
        assert!(prev.is_some())
    }
}

pub struct Parser<'cx> {
    arena: &'cx bumpalo::Bump,
    pub node_map: NodeMap<'cx>,
    pub parent_map: ParentMap,
    next_node_id: NodeID,
    pub atoms: AtomMap<'cx>,
}

impl<'cx> Parser<'cx> {
    pub fn new(ast_arena: &'cx bumpalo::Bump, mut atoms: AtomMap<'cx>) -> Self {
        assert!(ast_arena.allocation_limit().is_none());
        for (atom, id) in KEYWORDS {
            atoms.insert(*id, Cow::Borrowed(atom));
        }
        for (atom, id) in IDENTIFIER {
            atoms.insert(*id, Cow::Borrowed(atom))
        }
        Self {
            node_map: NodeMap(FxHashMap::default()),
            parent_map: ParentMap(FxHashMap::default()),
            next_node_id: NodeID::root(),
            atoms,
            arena: ast_arena,
        }
    }

    fn next_node_id(&mut self) -> NodeID {
        let old = self.next_node_id;
        self.next_node_id = self.next_node_id.next();
        old
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenValue {
    Number { value: f64 },
    Ident { value: AtomId },
}

impl TokenValue {
    fn number(self) -> f64 {
        if let TokenValue::Number { value } = self {
            value
        } else {
            unreachable!()
        }
    }

    fn ident(self) -> AtomId {
        if let TokenValue::Ident { value } = self {
            value
        } else {
            unreachable!()
        }
    }
}

pub struct ParserState<'cx, 'p> {
    p: &'p mut Parser<'cx>,
    input: &'p [u8],
    token: Token,
    token_value: Option<TokenValue>,
    pos: usize,
    parent: NodeID,
    module_id: ModuleID,
    ident_count: usize,
}

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub fn new(p: &'p mut Parser<'cx>, input: &'p [u8], module_id: ModuleID) -> Self {
        let token = Token::new(
            TokenKind::EOF,
            Span::new(u32::MAX, u32::MAX, ModuleID::root()),
        );
        Self {
            input,
            token,
            token_value: None,
            pos: 0,
            p,
            parent: NodeID::root(),
            module_id,
            ident_count: 0,
        }
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.p.arena.alloc(t)
    }

    #[inline]
    fn end(&self) -> usize {
        self.input.len()
    }

    fn parse_stmt(&mut self) -> &'cx ast::Stmt<'cx> {
        let id = self.p.next_node_id();
        let stmt = self.with_parent(id, |this| {
            use TokenKind::*;
            let kind = match this.token.kind {
                Var | Const => ast::StmtKind::Var(this.parse_var_stmt()),
                _ => ast::StmtKind::Expr(this.parse_expr_or_labeled_stmt().unwrap()),
            };
            let stmt = this.alloc(ast::Stmt { id, kind });
            stmt
        });
        self.insert_map(id, Node::Stmt(stmt));
        stmt
    }

    fn parse_var_stmt(&mut self) -> &'cx ast::VarStmt<'cx> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        use TokenKind::*;
        let kind = match self.token.kind {
            Var | Let | Const => unsafe {
                std::mem::transmute::<u8, ast::VarKind>(self.token.kind as u8 - Var as u8)
            },
            _ => unreachable!(),
        };
        let list = self.with_parent(id, Self::parse_var_decl_list);
        let span = self.new_span(start as usize, self.pos);
        let node = self.alloc(ast::VarStmt {
            id,
            kind,
            span,
            list,
        });
        self.insert_map(id, Node::VarStmt(node));
        self.parse_semi();
        node
    }

    fn parse_var_decl_list(&mut self) -> &'cx [&'cx ast::VarDecl<'cx>] {
        self.next_token();
        self.parse_delimited_list(
            |t| t.is_binding_ident_or_private_ident_or_pat(),
            Self::parse_var_decl,
            |t| t == TokenKind::Semi,
        )
    }

    fn parse_delimited_list<T>(
        &mut self,
        is_ele: impl Fn(TokenKind) -> bool,
        ele: impl Fn(&mut Self) -> PResult<T>,
        is_closing: impl Fn(TokenKind) -> bool,
    ) -> &'cx [T] {
        let mut list = vec![];
        while is_ele(self.token.kind) {
            if let Ok(ele) = ele(self) {
                list.push(ele);
            }
            if self.parse_optional(TokenKind::Comma) {
                continue;
            }
            if is_closing(self.token.kind) {
                break;
            }
        }
        self.alloc(list)
    }

    fn parse_union_or_intersection_ty(
        &mut self,
        parse_constituent_type: impl FnOnce(&mut Self) -> PResult<&'cx ast::Ty<'cx>>,
    ) -> PResult<&'cx ast::Ty<'cx>> {
        // let start = self.token.start();
        parse_constituent_type(self)
    }

    fn parse_intersection_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_union_or_intersection_ty(Self::parse_ty_op)
    }

    fn parse_union_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_union_or_intersection_ty(Self::parse_intersection_ty)
    }

    fn parse_ty_op(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_prefix_ty()
    }

    fn parse_prefix_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_non_array_ty()
    }

    fn parse_non_array_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        match self.token.kind {
            TokenKind::True | TokenKind::False => {
                todo!()
            }
            TokenKind::Ident => {
                let id = self.p.next_node_id();
                let ident = self.with_parent(id, |this| {
                    this.create_ident(this.ident_token(), this.token.span)
                });
                let ty = self.alloc(ast::Ty {
                    id,
                    kind: ast::TyKind::Ident(ident),
                });
                self.insert_map(id, Node::Ty(ty));
                Ok(ty)
            }
            _ => todo!(),
        }
    }

    fn parse_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_union_ty()
    }

    fn parse_ty_anno(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon) {
            self.parse_ty().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_var_decl(&mut self) -> PResult<&'cx ast::VarDecl<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.with_parent(id, |this| {
            let name = this.parse_ident_or_pat();
            // todo: parse type annotation
            let ty = this.parse_ty_anno()?;
            let init = this.parse_init();
            let span = this.new_span(start as usize, this.pos);
            let node = this.alloc(ast::VarDecl {
                id,
                span,
                name,
                ty,
                init,
            });
            this.insert_map(id, Node::VarDecl(node));
            Ok(node)
        })
    }

    fn parse_init(&mut self) -> Option<&'cx ast::Expr<'cx>> {
        self.parse_optional(TokenKind::Eq)
            .then(|| self.parse_assign_expr().unwrap())
    }

    fn parse_optional(&mut self, t: TokenKind) -> bool {
        if self.token.kind == t {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn parse_ident_or_pat(&mut self) -> &'cx ast::Ident {
        self.parse_binding_ident()
    }

    fn ident_token(&self) -> AtomId {
        assert!(
            matches!(self.token.kind, TokenKind::Ident),
            "{:#?}",
            self.token
        );
        self.token_value.unwrap().ident()
    }

    fn string_token(&self) -> AtomId {
        assert!(matches!(self.token.kind, TokenKind::String));
        self.token_value.unwrap().ident()
    }

    fn number_token(&self) -> f64 {
        assert!(matches!(self.token.kind, TokenKind::Number));
        self.token_value.unwrap().number()
    }

    fn parse_binding_ident(&mut self) -> &'cx ast::Ident {
        let name = self.ident_token();
        self.create_ident(name, self.token.span)
    }

    fn create_ident(&mut self, name: AtomId, span: Span) -> &'cx ast::Ident {
        self.ident_count += 1;
        let id = self.p.next_node_id();
        let ident = self.alloc(ast::Ident { id, name, span });
        self.next_token();
        self.insert_map(id, Node::Ident(ident));
        ident
    }

    fn parse_semi(&mut self) {
        if self.token.kind == TokenKind::Semi {
            self.next_token();
        }
    }

    fn parse_expr_or_labeled_stmt(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let expr = self.parse_expr();
        self.parse_semi();
        expr
    }

    fn parse_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_assign_expr()
    }

    fn parse_assign_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let expr = self.parse_binary_expr(BinPrec::Lowest);
        self.parse_cond_expr_rest(expr)
    }

    fn parse_cond_expr_rest(&mut self, cond: &'cx ast::Expr<'cx>) -> PResult<&'cx ast::Expr<'cx>> {
        if self.parse_optional(TokenKind::Question) {
            let start = cond.span().lo;
            let id = self.p.next_node_id();
            let kind = self.with_parent(id, |this| {
                let id = this.p.next_node_id();
                this.p.parent_map.r#override(cond.id, id);
                let when_true = this.with_parent(id, Self::parse_expr)?;
                this.expect(TokenKind::Colon)?;
                let when_false = this.with_parent(id, Self::parse_expr)?;
                let expr = this.alloc(ast::CondExpr {
                    id,
                    span: this.new_span(start as usize, this.pos),
                    cond,
                    when_false,
                    when_true,
                });
                this.insert_map(id, Node::CondExpr(expr));
                Ok(expr)
            })?;
            let expr = self.alloc(ast::Expr {
                id,
                kind: ast::ExprKind::Cond(kind),
            });
            self.insert_map(id, Node::Expr(expr));
            Ok(expr)
        } else {
            Ok(cond)
        }
    }

    fn parse_binary_expr(&mut self, prec: BinPrec) -> &'cx ast::Expr<'cx> {
        let start = self.token.start() as usize;
        let left = self.parse_unary_expr();
        self.parse_binary_expr_rest(prec, left, start)
    }

    fn parse_binary_expr_rest(
        &mut self,
        prec: BinPrec,
        left: &'cx ast::Expr<'_>,
        start: usize,
    ) -> &'cx ast::Expr<'cx> {
        let mut left = left;
        loop {
            let next_prec = self.token.kind.prec();
            if !(next_prec > prec) {
                break left;
            }
            let op = BinOp {
                kind: self.token.kind.into_binop(),
                span: self.token.span,
            };
            let bin_expr_id = self.p.next_node_id();
            self.next_token();
            self.p.parent_map.r#override(left.id, bin_expr_id);
            let right = self.with_parent(bin_expr_id, |this| this.parse_binary_expr(next_prec));
            let bin_expr = self.alloc(ast::BinExpr {
                id: bin_expr_id,
                left,
                op,
                right,
                span: self.new_span(start, right.span().hi as usize),
            });
            let expr_id = self.p.next_node_id();
            self.with_parent(expr_id, |this| {
                this.insert_map(bin_expr_id, Node::BinExpr(bin_expr));
            });
            let kind = ast::ExprKind::BinOp(bin_expr);
            left = self.alloc(ast::Expr { id: expr_id, kind });
            self.insert_map(expr_id, Node::Expr(left));
        }
    }

    fn parse_unary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        if self.is_update_expr() {
            let start = self.token.start();
            let expr = self.parse_update_expr();
            return expr;
        }
        todo!()
    }

    fn parse_update_expr(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_left_hand_side_expr()
    }

    fn parse_left_hand_side_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        self.parse_member_expr()
    }

    fn parse_member_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        // TODO: member expr rest
        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        use TokenKind::*;
        match self.token.kind {
            String | Number | True | False | Null => self.parse_lit(),
            LBracket => self.parse_array_lit(),
            LParen => self.parse_paren_expr().unwrap(),
            _ => self.parse_ident(),
        }
    }

    fn parse_paren_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let id = self.p.next_node_id();
        self.expect(TokenKind::LParen)?;
        let kind = self.with_parent(id, |this| {
            let id = this.p.next_node_id();
            let expr = this.parse_expr()?;
            this.expect(TokenKind::RParen)?;
            let expr = this.alloc(ast::ParenExpr {
                id,
                span: this.new_span(start as usize, this.pos),
                expr,
            });
            this.insert_map(id, Node::ParenExpr(expr));
            Ok(expr)
        })?;
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::Paren(kind),
        });
        Ok(expr)
    }

    fn expect(&mut self, t: TokenKind) -> PResult<()> {
        if self.token.kind == t {
            self.next_token();
            Ok(())
        } else {
            Err(())
        }
    }

    fn parse_array_lit(&mut self) -> &'cx ast::Expr<'cx> {
        let expr_id = self.p.next_node_id();
        let id = self.p.next_node_id();
        let start = self.token.start();
        if let Err(_) = self.expect(TokenKind::LBracket) {
            dbg!(self.token);
            todo!("error handler")
        }
        let elems = self.with_parent(id, Self::parse_array_lit_elems);
        if let Err(_) = self.expect(TokenKind::RBracket) {
            todo!("error handler")
        }
        let lit = self.alloc(ast::ArrayLit {
            id,
            span: self.new_span(start as usize, self.pos),
            elems,
        });
        self.with_parent(expr_id, |this| this.insert_map(id, Node::ArrayLit(&lit)));
        let expr = self.alloc(ast::Expr {
            id: expr_id,
            kind: ast::ExprKind::ArrayLit(lit),
        });
        expr
    }

    fn parse_array_lit_elems(&mut self) -> &'cx [&'cx ast::Expr<'cx>] {
        self.parse_delimited_list(
            |t| matches!(t, TokenKind::Comma) || t.is_start_of_expr(),
            |this| {
                if this.token.kind == TokenKind::Comma {
                    let id = this.p.next_node_id();
                    let kind = this.with_parent(id, |this| {
                        let id = this.p.next_node_id();
                        let expr = this.alloc(ast::OmitExpr {
                            id,
                            span: this.token.span,
                        });
                        this.insert_map(id, Node::OmitExpr(expr));
                        expr
                    });
                    let expr = this.alloc(ast::Expr {
                        id,
                        kind: ast::ExprKind::Omit(&kind),
                    });
                    this.insert_map(id, Node::Expr(expr));
                    Ok(expr)
                } else {
                    this.parse_assign_expr()
                }
            },
            |t| t == TokenKind::RBracket,
        )
    }

    fn parse_ident(&mut self) -> &'cx ast::Expr<'cx> {
        let id = self.p.next_node_id();
        let name = self.ident_token();
        let kind = self.with_parent(id, |this| this.create_ident(name, this.token.span));
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::Ident(kind),
        });
        self.insert_map(id, Node::Expr(expr));
        expr
    }

    fn parse_lit(&mut self) -> &'cx ast::Expr<'cx> {
        let id = self.p.next_node_id();
        let expr = self.with_parent(id, |this| {
            let kind = match this.token.kind {
                TokenKind::Number => {
                    let num = this.number_token();
                    let lit = this.create_lit(num, this.token.span);
                    this.insert_map(lit.id, Node::NumLit(lit));
                    ast::ExprKind::NumLit(lit)
                }
                TokenKind::False | TokenKind::True => {
                    let v = this.token.kind == TokenKind::True;
                    let lit = this.create_lit(v, this.token.span);
                    this.insert_map(lit.id, Node::BoolLit(lit));
                    ast::ExprKind::BoolLit(lit)
                }
                TokenKind::Null => {
                    let lit = this.create_lit((), this.token.span);
                    this.insert_map(lit.id, Node::NullLit(lit));
                    ast::ExprKind::NullLit(lit)
                }
                TokenKind::String => {
                    let s = this.string_token();
                    let lit = this.create_lit(s, this.token.span);
                    this.insert_map(lit.id, Node::StringLit(lit));
                    ast::ExprKind::StringLit(lit)
                }
                _ => unreachable!(),
            };
            let expr = this.alloc(ast::Expr { id, kind });
            expr
        });
        self.insert_map(id, Node::Expr(expr));
        expr
    }

    fn is_update_expr(&self) -> bool {
        true
    }

    fn create_lit<T>(&mut self, val: T, span: Span) -> &'cx ast::Lit<T> {
        let id = self.p.next_node_id();
        let lit = self.alloc(ast::Lit { id, val, span });
        self.next_token();
        lit
    }

    #[inline]
    fn with_parent<T>(&mut self, parent: NodeID, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.parent;
        self.parent = parent;
        let ret = f(self);
        self.parent = old;
        ret
    }

    fn insert_map(&mut self, id: NodeID, node: Node<'cx>) {
        self.p.node_map.insert(id, node);
        self.p.parent_map.insert(id, self.parent);
    }

    pub fn parse(&mut self) -> &'cx ast::Program<'cx> {
        let id = self.p.next_node_id();
        self.with_parent(id, |this| {
            this.next_token();
            let stmts = this.p.arena.alloc(Vec::with_capacity(32));
            while !matches!(this.token.kind, TokenKind::EOF) {
                stmts.push(this.parse_stmt());
            }
            let program = this.alloc(ast::Program { id, stmts });
            this.p.node_map.insert(id, Node::Program(program));
            program
        })
    }
}
