use bolt_ts_ast::TokenKind;
use bolt_ts_ast::{self as ast, NodeFlags};
use bolt_ts_span::Span;

use super::{PResult, ParserState};

pub(super) trait FnLike<'cx, 'p> {
    type Node;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        ms: enumflags2::BitFlags<ast::ModifierKind>,
    ) -> PResult<Option<&'cx ast::Ident>>;
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
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
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        ms: enumflags2::BitFlags<ast::ModifierKind>,
    ) -> PResult<Option<&'cx ast::Ident>> {
        if ms.contains(ast::ModifierKind::Default) {
            state.parse_optional_binding_ident()
        } else {
            Ok(Some(state.parse_binding_ident()))
        }
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> Self::Node {
        debug_assert!(
            name.is_some()
                || modifiers.is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Default))
        );
        let id = state.next_node_id();
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
        state.nodes.insert(decl.id, ast::Node::FnDecl(decl));
        decl
    }
}

pub(super) struct ParseFnExpr;
impl<'cx, 'p> FnLike<'cx, 'p> for ParseFnExpr {
    type Node = &'cx ast::FnExpr<'cx>;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        _: enumflags2::BitFlags<ast::ModifierKind>,
    ) -> PResult<Option<&'cx ast::Ident>> {
        // TODO: is_generator, is_async
        state.parse_optional_binding_ident()
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> Self::Node {
        assert!(modifiers.is_none());
        let id = state.next_node_id();
        let expr = state.alloc(ast::FnExpr {
            id,
            span,
            name,
            ty_params,
            params,
            ty,
            body: body.unwrap(),
        });
        state.nodes.insert(expr.id, ast::Node::FnExpr(expr));
        expr
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_fn_decl_or_expr<Node>(
        &mut self,
        mode: impl FnLike<'cx, 'p, Node = Node>,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<Node> {
        self.do_outside_of_context(
            NodeFlags::CLASS_FIELD_DEFINITION.union(NodeFlags::CLASS_STATIC_BLOCK),
            |this| {
                let start = this.token.start();
                this.expect(TokenKind::Function);
                let name =
                    mode.parse_name(this, modifiers.map(|ms| ms.flags).unwrap_or_default())?;
                let ty_params = this.parse_ty_params()?;
                let params = this.parse_params()?;
                this.check_params(params, false);
                let ret_ty = this.parse_fn_decl_ret_type()?;
                let body = this.do_outside_of_context(
                    NodeFlags::ALLOW_BREAK_CONTEXT.union(NodeFlags::ALLOW_CONTINUE_CONTEXT),
                    Self::parse_fn_block,
                )?;
                let span = this.new_span(start);
                Ok(mode.finish(this, span, modifiers, name, ty_params, params, ret_ty, body))
            },
        )
    }

    fn parse_fn_decl_ret_type(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon).is_some() {
            self.parse_ty_or_ty_pred().map(Some)
        } else {
            Ok(None)
        }
    }
}
