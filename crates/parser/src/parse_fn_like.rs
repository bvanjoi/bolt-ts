use bolt_ts_ast::TokenKind;
use bolt_ts_ast::{self as ast};
use bolt_ts_span::Span;

use super::SignatureFlags;
use super::parsing_ctx::ParseContext;
use super::{PResult, ParserState};

pub(super) trait FnLike<'cx, 'p> {
    type Node;
    type Modifier;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        ms: ast::ModifierFlags,
    ) -> PResult<Option<&'cx ast::Ident>>;
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        modifiers: Self::Modifier,
        asterisk_token: Option<Span>,
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
    type Modifier = Option<&'cx ast::Modifiers<'cx>>;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        ms: ast::ModifierFlags,
    ) -> PResult<Option<&'cx ast::Ident>> {
        if ms.contains(ast::ModifierFlags::DEFAULT) {
            state.parse_optional_binding_ident()
        } else {
            Ok(Some(state.parse_binding_ident()))
        }
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        modifiers: Self::Modifier,
        asterisk: Option<Span>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> Self::Node {
        debug_assert!(
            name.is_some()
                || modifiers.is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::DEFAULT))
        );
        let id = state.next_node_id();
        let decl = state.alloc(ast::FnDecl {
            id,
            span,
            modifiers,
            asterisk,
            name,
            ty_params,
            params,
            ty,
            body,
        });
        state.node_flags_map.insert(id, state.node_context_flags);
        state.set_external_module_indicator_if_has_export_mod(modifiers, id);
        state.nodes.insert(decl.id, ast::Node::FnDecl(decl));
        decl
    }
}

pub(super) struct ParseFnExpr;
impl<'cx, 'p> FnLike<'cx, 'p> for ParseFnExpr {
    type Node = &'cx ast::FnExpr<'cx>;
    type Modifier = Option<&'cx ast::Modifier>;
    fn parse_name(
        &self,
        state: &mut ParserState<'cx, 'p>,
        _: ast::ModifierFlags,
    ) -> PResult<Option<&'cx ast::Ident>> {
        // TODO: is_generator, is_async
        state.parse_optional_binding_ident()
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        async_modifier: Self::Modifier,
        asterisk: Option<Span>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> Self::Node {
        let id = state.next_node_id();
        let expr = state.alloc(ast::FnExpr {
            id,
            span,
            async_modifier,
            asterisk,
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
    pub(super) fn parse_fn_decl_or_expr<Node, Modifier>(
        &mut self,
        mode: impl FnLike<'cx, 'p, Node = Node, Modifier = Modifier>,
        modifiers: Modifier,
        modifier_flags: ast::ModifierFlags,
    ) -> PResult<Node> {
        self.do_outside_of_parse_context(
            ParseContext::CLASS_FIELD_DEFINITION.union(ParseContext::CLASS_STATIC_BLOCK),
            |this| {
                debug_assert!(this.token.kind == TokenKind::Function);
                let start = this.token.start();
                this.next_token(); // consume `function`
                let asterisk_token = this.parse_optional(TokenKind::Asterisk).map(|t| t.span);
                let name = mode.parse_name(this, modifier_flags)?;
                let ty_params = this.parse_ty_params();
                let params = this.parse_params();
                this.check_params::<false>(params);
                let ret_ty = this.parse_fn_decl_ret_type()?;
                let is_generator = asterisk_token.is_some();
                let is_async = modifier_flags.contains(ast::ModifierFlags::ASYNC);
                let flags = match (is_async, is_generator) {
                    (true, true) => SignatureFlags::YIELD
                        .union(SignatureFlags::ASYNC)
                        .union(SignatureFlags::AWAIT),
                    (true, false) => SignatureFlags::ASYNC.union(SignatureFlags::AWAIT),
                    (false, true) => SignatureFlags::YIELD,
                    (false, false) => SignatureFlags::empty(),
                };

                let body = this.parse_fn_block_or_semi(flags);
                let span = this.new_span(start);
                Ok(mode.finish(
                    this,
                    span,
                    modifiers,
                    asterisk_token,
                    name,
                    ty_params,
                    params,
                    ret_ty,
                    body,
                ))
            },
        )
    }

    fn parse_fn_decl_ret_type(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon).is_some() {
            self.parse_ty_or_ty_predicate().map(Some)
        } else {
            Ok(None)
        }
    }
}
