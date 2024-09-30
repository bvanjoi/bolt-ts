mod node_flags;
mod scan;
mod token;

use std::borrow::Cow;
use std::u32;

use rts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;
use token::{BinPrec, Token, TokenKind};

use crate::ast::{self, BinOp, Node, NodeID};
use crate::atoms::{AtomId, AtomMap};
use crate::keyword::{IDENTIFIER, KEYWORDS};

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
    pub fn new(ast_arena: &'cx bumpalo::Bump) -> Self {
        assert!(ast_arena.allocation_limit().is_none());
        let mut atoms = AtomMap::default();
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
                Var => ast::StmtKind::Var(this.parse_var_stmt()),
                _ => ast::StmtKind::Expr(this.parse_expr_or_labeled_stmt()),
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
        let list = self.with_parent(id, |this| this.parse_var_decl_list());
        let span = self.new_span(start as usize, self.pos);
        let node = self.alloc(ast::VarStmt { id, span, list });
        self.insert_map(id, Node::VarStmt(node));
        self.parse_semi();
        node
    }

    fn parse_var_decl_list(&mut self) -> &'cx [&'cx ast::VarDecl<'cx>] {
        use TokenKind::*;
        match self.token.kind {
            Var => (),
            _ => unreachable!(),
        }
        self.next_token();
        self.parse_delimited_list()
    }

    fn parse_delimited_list(&mut self) -> &'cx [&'cx ast::VarDecl<'cx>] {
        let mut list = vec![];
        loop {
            list.push(self.parse_var_decl());
            if self.token.kind != TokenKind::Comma {
                break;
            }
        }
        self.alloc(list)
    }

    fn parse_var_decl(&mut self) -> &'cx ast::VarDecl<'cx> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.with_parent(id, |this| {
            let name = this.parse_ident_or_pat();
            // todo: parse type annotation
            let init = this.parse_init();
            let span = this.new_span(start as usize, this.pos);
            let node = this.alloc(ast::VarDecl {
                id,
                span,
                name,
                init,
            });
            this.insert_map(id, Node::VarDecl(node));
            node
        })
    }

    fn parse_init(&mut self) -> Option<&'cx ast::Expr<'cx>> {
        self.parse_optional(TokenKind::Eq)
            .then(|| self.parse_assign_expr())
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
        assert!(matches!(self.token.kind, TokenKind::Ident));
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

    fn parse_expr_or_labeled_stmt(&mut self) -> &'cx ast::Expr<'cx> {
        let expr = self.parse_expr();
        self.parse_semi();
        expr
    }

    fn parse_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let expr = self.parse_assign_expr();
        expr
    }

    fn parse_assign_expr(&mut self) -> &'cx ast::Expr<'cx> {
        self.parse_binary_expr(BinPrec::Lowest)
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
            String | Number | False | Null => self.parse_literal(),
            _ => self.parse_ident(),
        }
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

    fn parse_literal(&mut self) -> &'cx ast::Expr<'cx> {
        let id = self.p.next_node_id();
        let expr = self.with_parent(id, |this| {
            let kind = match this.token.kind {
                TokenKind::Number => {
                    let num = this.number_token();
                    let lit = this.create_lit(num, this.token.span);
                    this.insert_map(lit.id, Node::NumLit(lit));
                    ast::ExprKind::NumLit(lit)
                }
                TokenKind::False => {
                    let lit = this.create_lit(false, this.token.span);
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
