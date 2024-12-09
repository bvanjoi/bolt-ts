use bolt_ts_span::Span;

use super::ast;
use super::token::TokenKind;
use super::{PResult, ParserState};

pub(super) trait FnLike<'p> {
    type Node;
    fn parse_name(&self, state: &mut ParserState<'p>) -> PResult<Option<&'p ast::Ident>>;
    fn finish(
        self,
        state: &mut ParserState<'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'p ast::Modifiers<'p>>,
        name: Option<&'p ast::Ident>,
        ty_params: Option<ast::TyParams<'p>>,
        params: ast::ParamsDecl<'p>,
        ret_ty: Option<&'p ast::Ty<'p>>,
        body: Option<&'p ast::BlockStmt<'p>>,
    ) -> Self::Node;
}

pub(super) struct ParseFnDecl;
impl<'p> FnLike<'p> for ParseFnDecl {
    type Node = &'p ast::FnDecl<'p>;
    fn parse_name(&self, state: &mut ParserState<'p>) -> PResult<Option<&'p ast::Ident>> {
        Ok(Some(state.parse_binding_ident()))
    }
    fn finish(
        self,
        state: &mut ParserState<'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'p ast::Modifiers<'p>>,
        name: Option<&'p ast::Ident>,
        ty_params: Option<ast::TyParams<'p>>,
        params: ast::ParamsDecl<'p>,
        ret_ty: Option<&'p ast::Ty<'p>>,
        body: Option<&'p ast::BlockStmt<'p>>,
    ) -> Self::Node {
        let name = name.unwrap();
        let decl = state.alloc(ast::FnDecl {
            id,
            span,
            modifiers,
            name,
            ty_params,
            params,
            ret_ty,
            body,
        });
        state.insert_map(decl.id, ast::Node::FnDecl(decl));
        decl
    }
}

pub(super) struct ParseFnExpr;
impl<'p> FnLike<'p> for ParseFnExpr {
    type Node = &'p ast::FnExpr<'p>;
    fn parse_name(&self, state: &mut ParserState<'p>) -> PResult<Option<&'p ast::Ident>> {
        Ok(
            (state.token.kind.is_binding_ident() && !state.is_implements_clause())
                .then(|| state.parse_binding_ident()),
        )
    }
    fn finish(
        self,
        state: &mut ParserState<'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'p ast::Modifiers<'p>>,
        name: Option<&'p ast::Ident>,
        ty_params: Option<ast::TyParams<'p>>,
        params: ast::ParamsDecl<'p>,
        ret_ty: Option<&'p ast::Ty<'p>>,
        body: Option<&'p ast::BlockStmt<'p>>,
    ) -> Self::Node {
        assert!(modifiers.is_none());
        let expr = state.alloc(ast::FnExpr {
            id,
            span,
            name,
            ty_params,
            params,
            ret_ty,
            body: body.unwrap(),
        });
        state.insert_map(expr.id, ast::Node::FnExpr(expr));
        expr
    }
}

impl<'p> ParserState<'p> {
    pub(super) fn parse_fn_decl_or_expr<Node>(
        &mut self,
        mode: impl FnLike<'p, Node = Node>,
        modifiers: Option<&'p ast::Modifiers<'p>>,
    ) -> PResult<Node> {
        use TokenKind::*;
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(Function)?;
        let name = self.with_parent(id, |this| mode.parse_name(this))?;
        let ty_params = self.with_parent(id, Self::parse_ty_params)?;
        let params = self.with_parent(id, Self::parse_params)?;
        let ret_ty = self.with_parent(id, Self::parse_fn_decl_ret_type)?;
        let body = self.with_parent(id, Self::parse_fn_block)?;
        let span = self.new_span(start as usize, self.pos);
        Ok(mode.finish(
            self, id, span, modifiers, name, ty_params, params, ret_ty, body,
        ))
    }

    fn parse_fn_decl_ret_type(&mut self) -> PResult<Option<&'p ast::Ty<'p>>> {
        if self.parse_optional(TokenKind::Colon).is_some() {
            self.parse_ty_or_ty_pred().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }
}
