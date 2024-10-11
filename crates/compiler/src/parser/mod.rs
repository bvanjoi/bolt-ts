mod list_ctx;
mod scan;
mod token;
mod ty;

use std::borrow::Cow;
use std::u32;

use list_ctx::ListContext;
use rts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;
use token::{BinPrec, Token, TokenKind};

use crate::ast::{self, BinOp, Node, NodeID};
use crate::atoms::{AtomId, AtomMap};
use crate::keyword;

type PResult<T> = Result<T, ()>;

pub struct Nodes<'cx>(FxHashMap<NodeID, Node<'cx>>);

impl<'cx> Nodes<'cx> {
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
    pub nodes: Nodes<'cx>,
    pub parent_map: ParentMap,
    next_node_id: NodeID,
    pub atoms: AtomMap<'cx>,
}

impl<'cx> Parser<'cx> {
    pub fn new(ast_arena: &'cx bumpalo::Bump, mut atoms: AtomMap<'cx>) -> Self {
        assert!(ast_arena.allocation_limit().is_none());
        for (atom, id) in keyword::KEYWORDS {
            atoms.insert(*id, Cow::Borrowed(atom));
        }
        for (atom, id) in keyword::IDENTIFIER {
            atoms.insert(*id, Cow::Borrowed(atom))
        }
        Self {
            nodes: Nodes(FxHashMap::default()),
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

    fn parse_stmt(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        let id = self.p.next_node_id();
        let stmt = self.with_parent(id, |this| {
            use TokenKind::*;
            let kind = match this.token.kind {
                Semi => ast::StmtKind::Empty(this.parse_empty_stmt()?),
                Var | Let | Const => ast::StmtKind::Var(this.parse_var_stmt()),
                Function => ast::StmtKind::Fn(this.parse_fn_decl()?),
                If => ast::StmtKind::If(this.parse_if_stmt()?),
                LBrace => ast::StmtKind::Block(this.parse_block()?),
                Return => ast::StmtKind::Return(this.parse_ret_stmt()?),
                _ => ast::StmtKind::Expr(this.parse_expr_or_labeled_stmt()?),
            };
            let stmt = this.alloc(ast::Stmt { id, kind });
            Ok(stmt)
        })?;
        self.insert_map(id, Node::Stmt(stmt));
        Ok(stmt)
    }

    fn parse_empty_stmt(&mut self) -> PResult<&'cx ast::EmptyStmt> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Semi);
        let stmt = self.alloc(ast::EmptyStmt {
            id,
            span: self.new_span(start as usize, self.pos),
        });
        self.insert_map(id, Node::EmptyStmt(stmt));
        Ok(stmt)
    }

    fn can_parse_semi(&self) -> bool {
        if self.token.kind == TokenKind::Semi {
            true
        } else {
            self.token.kind == TokenKind::RBrace || self.token.kind == TokenKind::EOF
        }
    }

    fn parse_ret_stmt(&mut self) -> PResult<&'cx ast::RetStmt<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Return);
        let expr = if self.can_parse_semi() {
            None
        } else {
            Some(self.with_parent(id, Self::parse_expr)?)
        };
        self.parse_semi();
        let stmt = self.alloc(ast::RetStmt {
            id,
            span: self.new_span(start as usize, self.pos),
            expr,
        });
        self.insert_map(id, Node::RetStmt(stmt));
        Ok(stmt)
    }

    fn parse_if_stmt(&mut self) -> PResult<&'cx ast::IfStmt<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LParen)?;
        let expr = self.with_parent(id, Self::parse_expr)?;
        self.expect(TokenKind::RParen)?;
        let then = self.with_parent(id, Self::parse_stmt)?;
        let else_then = if self.parse_optional(TokenKind::Else) {
            Some(self.with_parent(id, Self::parse_stmt)?)
        } else {
            None
        };
        let stmt = self.alloc(ast::IfStmt {
            id,
            span: self.new_span(start as usize, self.pos),
            expr,
            then,
            else_then,
        });
        self.insert_map(id, Node::IfStmt(stmt));

        Ok(stmt)
    }

    fn parse_fn_decl(&mut self) -> PResult<&'cx ast::FnDecl<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Function)?;
        let name = self.with_parent(id, Self::parse_binding_ident);
        // TODO: type params
        let params = self.with_parent(id, Self::parse_params)?;
        let ret_ty = self.with_parent(id, Self::parse_fn_decl_ret_type)?;
        let body = self.parse_fn_body()?;
        let f = self.alloc(ast::FnDecl {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            params,
            ret_ty,
            body,
        });
        self.insert_map(id, Node::FnDecl(f));
        Ok(f)
    }

    fn parse_fn_decl_ret_type(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon) {
            self.parse_ty_or_ty_pred().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_fn_body(&mut self) -> PResult<&'cx [&'cx ast::Stmt<'cx>]> {
        self.parse_block()
    }

    fn parse_block(&mut self) -> PResult<&'cx [&'cx ast::Stmt<'cx>]> {
        use TokenKind::*;
        self.expect(LBrace)?;
        let list = self.parse_list(
            list_ctx::BlockStmt::is_ele,
            Self::parse_stmt,
            list_ctx::BlockStmt::is_closing,
        );
        self.expect(RBrace)?;
        Ok(list)
    }

    fn parse_list<T>(
        &mut self,
        is_ele: impl Fn(TokenKind) -> bool,
        ele: impl Fn(&mut Self) -> PResult<T>,
        is_closing: impl Fn(TokenKind) -> bool,
    ) -> &'cx [T] {
        let mut list = vec![];
        while !is_closing(self.token.kind) {
            if is_ele(self.token.kind) {
                if let Ok(ele) = ele(self) {
                    list.push(ele);
                }
            }
        }
        self.alloc(list)
    }

    fn parse_params(&mut self) -> PResult<ast::ParamsDecl<'cx>> {
        use TokenKind::*;
        self.expect(LParen)?;
        let params = self.parse_delimited_list(
            |t| t.is_start_of_param(),
            Self::parse_param,
            |t| matches!(t, RParen | RBracket),
        );
        self.expect(RParen)?;
        Ok(params)
    }

    fn parse_param(&mut self) -> PResult<&'cx ast::ParamDecl<'cx>> {
        let start = self.token.start();
        let id = self.p.next_node_id();
        let name = self.with_parent(id, Self::parse_ident_name)?;
        let ty = self.with_parent(id, Self::parse_ty_anno)?;
        let init = self.with_parent(id, Self::parse_init);
        let decl = self.alloc(ast::ParamDecl {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            ty,
            init,
        });
        self.insert_map(id, Node::ParamDecl(decl));
        Ok(decl)
    }

    fn parse_name_of_param(&mut self) -> PResult<&'cx ast::Ident> {
        let name = self.parse_ident_or_pat();
        Ok(name)
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

    fn scan_speculation_helper<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
        is_lookahead: bool,
    ) -> T {
        let old_pos = self.pos;
        let old_token = self.token;
        let old_token_value = self.token_value;

        let r = f(self);

        if is_lookahead {
            self.pos = old_pos;
            self.token = old_token;
            self.token_value = old_token_value;
        }
        r
    }

    fn scan_lookahead<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scan_speculation_helper(f, true)
    }

    fn speculation_helper<T>(&mut self, f: impl FnOnce(&mut Self) -> T, try_parse: bool) -> T {
        let old_token = self.token;

        let r = if try_parse {
            todo!()
        } else {
            self.scan_lookahead(f)
        };

        if !try_parse {
            self.token = old_token;
        }

        r
    }

    fn lookahead<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.speculation_helper(f, false)
    }

    fn is_start_of_fn_or_ctor_ty(&mut self) -> bool {
        let t = self.token.kind;
        if t == TokenKind::LParen && self.lookahead(Self::is_unambiguously_start_of_fn_ty) {
            true
        } else {
            false
        }
    }

    fn is_unambiguously_start_of_fn_ty(&mut self) -> bool {
        self.next_token();
        let t = self.token.kind;
        if t == TokenKind::RParen {
            true
        } else {
            false
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
        assert!(matches!(
            self.token.kind,
            TokenKind::String | TokenKind::NoSubstitutionTemplate
        ));
        self.token_value.unwrap().ident()
    }

    fn number_token(&self) -> f64 {
        assert!(matches!(self.token.kind, TokenKind::Number));
        self.token_value.unwrap().number()
    }

    fn parse_binding_ident(&mut self) -> &'cx ast::Ident {
        self.create_ident(true)
    }

    fn create_ident(&mut self, is_ident: bool) -> &'cx ast::Ident {
        if is_ident {
            self.ident_count += 1;
            let id = self.p.next_node_id();
            let name = self.ident_token();
            let span = self.token.span;
            let ident = self.alloc(ast::Ident { id, name, span });
            self.next_token();
            self.insert_map(id, Node::Ident(ident));
            ident
        } else {
            unreachable!()
        }
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
        let start = self.token.start();
        let expr = self.parse_left_hand_side_expr();
        self.parse_call_expr(start as usize, expr).unwrap()
    }

    fn parse_call_expr(
        &mut self,
        start: usize,
        mut expr: &'cx ast::Expr<'cx>,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        loop {
            expr = self.parse_member_expr_rest(start, expr)?;
            if self.token.kind == TokenKind::LParen {
                let id = self.p.next_node_id();
                self.p.parent_map.r#override(expr.id, id);
                let kind = self.with_parent(id, |this| {
                    let id = this.p.next_node_id();
                    let args = this.with_parent(id, Self::parse_args)?;
                    let call = this.alloc(ast::CallExpr {
                        id,
                        span: this.new_span(start, this.pos),
                        expr,
                        args,
                    });
                    this.insert_map(id, Node::CallExpr(call));
                    Ok(call)
                })?;
                expr = self.alloc(ast::Expr {
                    id,
                    kind: ast::ExprKind::Call(kind),
                });
                self.insert_map(id, Node::Expr(expr));
            } else {
                break Ok(expr);
            }
        }
    }

    fn parse_args(&mut self) -> PResult<&'cx [&'cx ast::Expr<'cx>]> {
        self.expect(TokenKind::LParen)?;
        let args = self.parse_delimited_list(
            list_ctx::ArgumentExpressions::is_ele,
            Self::parse_arg,
            list_ctx::ArgumentExpressions::is_closing,
        );
        self.expect(TokenKind::RParen)?;
        Ok(args)
    }

    fn parse_arg(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_arg_or_array_lit_elem()
    }

    fn parse_arg_or_array_lit_elem(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_assign_expr()
    }

    fn parse_left_hand_side_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        self.parse_member_expr()
    }

    fn parse_member_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        let expr = self.parse_primary_expr();
        self.parse_member_expr_rest(start as usize, expr).unwrap()
    }

    fn parse_member_expr_rest(
        &mut self,
        start: usize,
        expr: &'cx ast::Expr<'cx>,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        use TokenKind::*;
        match self.token.kind {
            NoSubstitutionTemplate | String | Number | True | False | Null => self.parse_lit(),
            LBracket => self.parse_array_lit(),
            LParen => self.parse_paren_expr().unwrap(),
            LBrace => self.parse_object_lit().unwrap(),
            _ => self.parse_ident(),
        }
    }

    fn parse_object_lit(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(LBrace)?;
        let kind = self.with_parent(id, |this| {
            let id = this.p.next_node_id();
            let props = this.parse_delimited_list(
                |t| matches!(t, LBrace) || t.is_lit_prop_name(),
                Self::parse_object_lit_ele,
                |t| matches!(t, RBrace),
            );
            this.expect(RBrace)?;
            let lit = this.alloc(ast::ObjectLit {
                id,
                span: this.new_span(start as usize, this.pos),
                members: props,
            });
            this.insert_map(id, Node::ObjectLit(lit));
            Ok(lit)
        })?;
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::ObjectLit(kind),
        });
        self.insert_map(id, Node::Expr(expr));
        Ok(expr)
    }

    fn is_ident(&self) -> bool {
        matches!(self.token.kind, TokenKind::Ident)
    }

    fn parse_object_lit_ele(&mut self) -> PResult<&'cx ast::ObjectMemberField<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        // let mods = self.with_parent(id, Self::parse_modifiers)?;
        // let is_ident = self.is_ident();
        let name = self.with_parent(id, Self::parse_prop_name)?;
        self.parse_optional(TokenKind::Question);
        self.expect(TokenKind::Colon)?;
        let value = self.with_parent(id, Self::parse_assign_expr)?;
        let filed = self.alloc(ast::ObjectMemberField {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            value,
        });
        self.insert_map(id, Node::ObjectMemberField(filed));
        Ok(filed)
    }

    fn parse_prop_name(&mut self) -> PResult<&'cx ast::PropName<'cx>> {
        let id = self.p.next_node_id();
        let ident = self.with_parent(id, Self::parse_ident_name)?;
        let prop_name = self.alloc(ast::PropName {
            id,
            kind: ast::PropNameKind::Ident(ident),
        });
        self.insert_map(id, Node::PropName(prop_name));
        Ok(prop_name)
    }

    fn parse_ident_name(&mut self) -> PResult<&'cx ast::Ident> {
        Ok(self.create_ident(true))
    }

    fn parse_modifiers(&mut self) -> PResult<Option<()>> {
        loop {
            let Ok(Some(m)) = self.parse_modifier() else {
                break;
            };
        }
        Ok(None)
    }

    fn parse_modifier(&mut self) -> PResult<Option<()>> {
        Ok(None)
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
            dbg!(self.token);
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
        let kind = self.with_parent(id, |this| this.create_ident(true));
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
            use TokenKind::*;
            let kind = match this.token.kind {
                Number => {
                    let num = this.number_token();
                    let lit = this.create_lit(num, this.token.span);
                    this.insert_map(lit.id, Node::NumLit(lit));
                    ast::ExprKind::NumLit(lit)
                }
                False | True => {
                    let v = this.token.kind == True;
                    let lit = this.create_lit(v, this.token.span);
                    this.insert_map(lit.id, Node::BoolLit(lit));
                    ast::ExprKind::BoolLit(lit)
                }
                Null => {
                    let lit = this.create_lit((), this.token.span);
                    this.insert_map(lit.id, Node::NullLit(lit));
                    ast::ExprKind::NullLit(lit)
                }
                String | NoSubstitutionTemplate => {
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
        self.p.nodes.insert(id, node);
        self.p.parent_map.insert(id, self.parent);
    }

    pub fn parse(&mut self) -> &'cx ast::Program<'cx> {
        let id = self.p.next_node_id();
        self.with_parent(id, |this| {
            this.next_token();
            let stmts = this.p.arena.alloc(Vec::with_capacity(32));
            while !matches!(this.token.kind, TokenKind::EOF) {
                if let Ok(stmt) = this.parse_stmt() {
                    stmts.push(stmt);
                }
            }
            let program = this.alloc(ast::Program { id, stmts });
            this.p.nodes.insert(id, Node::Program(program));
            program
        })
    }
}
