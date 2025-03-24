use bolt_ts_ast as ast;
use bolt_ts_ast::TokenKind;
use bolt_ts_span::Span;

use super::{PResult, ParserState};

pub(super) trait FnLike<'cx, 'p> {
    type Node;
    fn parse_name(&self, state: &mut ParserState<'cx, 'p>) -> PResult<Option<&'cx ast::Ident>>;
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ret_ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> Self::Node;
}

pub(super) struct ParseFnDecl;
impl<'cx, 'p> FnLike<'cx, 'p> for ParseFnDecl {
    type Node = &'cx ast::FnDecl<'cx>;
    fn parse_name(&self, state: &mut ParserState<'cx, 'p>) -> PResult<Option<&'cx ast::Ident>> {
        Ok(Some(state.parse_binding_ident()))
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> Self::Node {
        let name = name.unwrap();
        let decl = state.alloc(ast::FnDecl {
            id,
            span,
            modifiers,
            name,
            ty_params,
            params,
            ty,
            body,
        });
        state.node_flags_map.insert(id, state.context_flags);
        state.set_external_module_indicator_if_has_export_mod(modifiers, id);
        state.insert_map(decl.id, ast::Node::FnDecl(decl));
        decl
    }
}

pub(super) struct ParseFnExpr;
impl<'cx, 'p> FnLike<'cx, 'p> for ParseFnExpr {
    type Node = &'cx ast::FnExpr<'cx>;
    fn parse_name(&self, state: &mut ParserState<'cx, 'p>) -> PResult<Option<&'cx ast::Ident>> {
        Ok(
            (state.token.kind.is_binding_ident() && !state.is_implements_clause())
                .then(|| state.parse_binding_ident()),
        )
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> Self::Node {
        assert!(modifiers.is_none());
        let expr = state.alloc(ast::FnExpr {
            id,
            span,
            name,
            ty_params,
            params,
            ty,
            body: body.unwrap(),
        });
        state.insert_map(expr.id, ast::Node::FnExpr(expr));
        expr
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_fn_decl_or_expr<Node>(
        &mut self,
        mode: impl FnLike<'cx, 'p, Node = Node>,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<Node> {
        use bolt_ts_ast::TokenKind::*;
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(Function);
        let name = mode.parse_name(self)?;
        let ty_params = self.parse_ty_params()?;
        let params = self.parse_params()?;
        let ret_ty = self.parse_fn_decl_ret_type()?;
        let body = self.parse_fn_block()?;
        let span = self.new_span(start);
        Ok(mode.finish(
            self, id, span, modifiers, name, ty_params, params, ret_ty, body,
        ))
    }

    fn parse_fn_decl_ret_type(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon).is_some() {
            self.parse_ty_or_ty_pred().map(Some)
        } else {
            Ok(None)
        }
    }
}
