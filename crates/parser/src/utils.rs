use bolt_ts_ast::{TokenFlags, TokenKind};
use bolt_ts_ast_factory::ASTFactory;
use bolt_ts_span::Span;

use super::lookahead::Lookahead;
use super::parsing_ctx::{ParseContext, ParsingContext};
use super::{PResult, ParserState};
use super::{SignatureFlags, keyword};
use super::{ast, errors};

pub(super) trait ParseSuccess {
    fn is_success(&self) -> bool;
}

impl ParseSuccess for bool {
    fn is_success(&self) -> bool {
        *self
    }
}

impl<T, E> ParseSuccess for Result<Option<T>, E> {
    fn is_success(&self) -> bool {
        self.as_ref().is_ok_and(|x| x.is_some())
    }
}

impl<'cx> ParserState<'cx, '_> {
    pub(super) fn parse_fn_block_or_semi(
        &mut self,
        flags: SignatureFlags,
    ) -> Option<&'cx ast::BlockStmt<'cx>> {
        if self.token.kind != TokenKind::LBrace {
            if flags.contains(SignatureFlags::TYPE) {
                self.parse_ty_member_semi();
                return None;
            } else if self.can_parse_semi() {
                self.parse_semi();
                return None;
            }
        }
        Some(self.parse_fn_block(flags))
    }

    pub(super) fn parse_fn_block(&mut self, flags: SignatureFlags) -> &'cx ast::BlockStmt<'cx> {
        let saved_yield_context = self.in_yield_context();
        let saved_await_context = self.in_await_context();

        let saved_labels = std::mem::take(&mut self.labels);
        self.set_yield_context(flags.contains(SignatureFlags::YIELD));
        self.set_await_context(flags.contains(SignatureFlags::AWAIT));

        let ret = self.do_outside_of_parse_context(
            ParseContext::ALLOW_BREAK
                .union(ParseContext::ALLOW_CONTINUE)
                .union(ParseContext::MODULE_BLOCK),
            |this| {
                let context = if flags.contains(SignatureFlags::ASYNC) {
                    ParseContext::FN_BLOCK.union(ParseContext::ASYNC)
                } else {
                    ParseContext::FN_BLOCK
                };
                this.do_inside_of_parse_context(context, Self::parse_block)
            },
        );

        self.labels = saved_labels;
        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        ret
    }

    pub(super) fn parse_expected_matching_brackets(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        open_parsed: bool,
        open_pos: usize,
    ) {
        if self.token.kind == close {
            self.next_token();
            return;
        }

        let mut expected_error = errors::KindExpected {
            span: self.token.span,
            expected: close.as_str().to_string(),
            related: None,
        };

        if open_parsed {
            expected_error.related = Some(errors::ExpectedToFindAToMatchTheBTokenHere {
                span: Span::new(open_pos as u32, open_pos as u32 + 1, self.module_id),
                expected: open.as_str().to_string(),
                found: close.as_str().to_string(),
            });
        }
        self.push_error(Box::new(expected_error));
    }

    pub(super) fn parse_block(&mut self) -> &'cx ast::BlockStmt<'cx> {
        let start = self.token.start();
        use bolt_ts_ast::TokenKind::*;
        let open = LBrace;
        let open_brace_parsed = self.expect(LBrace);
        let saved_external_module_indicator = self.external_module_indicator;
        let stmts = self.do_inside_of_parse_context(ParseContext::BLOCK, |this| {
            this.parse_list(ParsingContext::BLOCK_STATEMENTS, Self::parse_stmt)
        });
        self.external_module_indicator = saved_external_module_indicator;
        self.parse_expected_matching_brackets(open, RBrace, open_brace_parsed, start as usize);
        let id = self.next_node_id();
        let stmt = self.alloc(ast::BlockStmt {
            id,
            span: self.new_span(start),
            stmts,
        });
        self.nodes.insert(id, ast::Node::BlockStmt(stmt));
        stmt
    }

    pub(super) fn parse_ty_params(&mut self) -> Option<ast::TyParams<'cx>> {
        if self.token.kind == TokenKind::Less {
            let less_token_span = self.token.span;
            let ty_params = self.parse_bracketed_list::<false, _>(
                ParsingContext::TYPE_PARAMETERS,
                TokenKind::Less,
                Self::parse_ty_param,
                TokenKind::Great,
            );
            if ty_params.is_empty() {
                let error = errors::TypeParameterListCannotBeEmpty {
                    span: less_token_span,
                };
                self.push_error(Box::new(error));
                None
            } else {
                Some(ty_params)
            }
        } else {
            None
        }
    }

    pub(super) fn is_start_of_param(&mut self) -> bool {
        let t = self.token.kind;
        matches!(t, TokenKind::DotDotDot)
            || t.is_binding_ident_or_private_ident_or_pat()
            || t.is_modifier_kind()
            || t == TokenKind::At
            || self.is_start_of_ty(true)
    }

    pub(super) fn is_start_of_ty(&mut self, in_start_of_param: bool) -> bool {
        use bolt_ts_ast::TokenKind::*;
        if matches!(
            self.token.kind,
            Readonly
                | Unique
                | Void
                | Undefined
                | Null
                | Typeof
                | LBrace
                | LBracket
                | Pipe
                | Amp
                | This
                | Type
                | Less
                | New
                | String
                | Number
                | BigInt
                | True
                | False
                | Asterisk
                | Question
                | Excl
                | DotDotDot
                | Infer
                | Import
                | NoSubstitutionTemplate
                | TemplateHead
        ) {
            true
        } else if self.token.kind == TokenKind::Function {
            !in_start_of_param
        } else if self.token.kind == TokenKind::LParen && !in_start_of_param {
            self.lookahead(|this| {
                this.p().next_token();
                let t = this.p().token.kind;
                t == TokenKind::RParen
                    || this.p().is_start_of_param()
                    || this.p().is_start_of_ty(false)
            })
        } else if self.token.kind == TokenKind::Minus {
            !in_start_of_param
                && self.lookahead(Lookahead::next_token_is_numeric_or_big_int_literal)
        } else {
            self.is_ident()
        }
    }

    pub(super) fn is_start_of_left_hand_side_expr(&mut self) -> bool {
        use TokenKind::*;
        match self.token.kind {
            This
            | Super
            | Null
            | True
            | False
            | Number
            | BigInt
            | String
            | NoSubstitutionTemplate
            | LBrace
            | LBracket
            | LParen
            | Function
            | Class
            | New
            | Slash
            | SlashEq
            | Ident
            | TemplateHead => true,
            Import => self.lookahead(Lookahead::next_token_is_lparen_or_less_or_dot),
            _ => self.is_ident(),
        }
    }

    pub(super) fn is_start_of_expr(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        self.is_start_of_left_hand_side_expr()
            || matches!(
                self.token.kind,
                Plus | Minus
                    | Tilde
                    | Excl
                    | Delete
                    | Typeof
                    | Void
                    | PlusPlus
                    | MinusMinus
                    | Less
                    | Await
                    | Yield
                    | Private
                    | At
                    | Pipe
            )
    }

    pub(super) fn is_start_of_stmt(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        let t = self.token.kind;
        if matches!(t, Export | Const) {
            self.is_start_of_decl()
        } else if t == Import {
            self.is_start_of_decl()
                || self.lookahead(Lookahead::next_token_is_lparen_or_less_or_dot)
        } else if matches!(
            t,
            Semi | LBrace
                    | Var
                    | Let
                    | Function
                    | Class
                    | Enum
                    | If
                    | Do
                    | While
                    | For
                    | Continue
                    | Break
                    | Return
                    | Switch
                    | Throw
                    | Try
                    | Debugger
                    | Catch
                    | Finally
                    // ==
                    | Interface
                    | Module
                    | Namespace
                    | Type
        ) {
            // TODO: global, defer
            true
        } else if matches!(t, Public | Private | Protected | Static | Readonly) {
            self.is_start_of_decl()
                || !self.lookahead(Lookahead::next_token_is_ident_or_keyword_on_same_line)
        } else {
            self.is_start_of_expr()
        }
    }

    fn parse_const_modifier(&mut self) -> Option<&'cx ast::Modifier> {
        if self.token.kind == TokenKind::Const {
            let span = self.token.span;
            self.next_token();
            let kind = ast::ModifierKind::Const;
            Some(self.create_modifier(span, kind))
        } else {
            None
        }
    }

    fn parse_ty_param(&mut self) -> PResult<&'cx ast::TyParam<'cx>> {
        let start = self.token.start();
        let const_modifier = self.parse_const_modifier();
        let name = self.parse_binding_ident();
        let constraint = if self.parse_optional(TokenKind::Extends).is_some() {
            if self.is_start_of_ty(false) || !self.is_start_of_expr() {
                Some(self.parse_ty()?)
            } else {
                todo!("token: {:#?}", self.token.kind)
            }
        } else {
            None
        };
        let default = if self.parse_optional(TokenKind::Eq).is_some() {
            Some(self.parse_ty()?)
        } else {
            None
        };
        let span = self.new_span(start);
        let const_modifier = const_modifier.map(|m| m.span());
        Ok(self.create_type_parameter(span, const_modifier, name, constraint, default))
    }

    pub(super) fn parse_ident(
        &mut self,
        missing_ident_kind: Option<errors::MissingIdentKind>,
    ) -> &'cx ast::Expr<'cx> {
        let ident = self.create_ident(self.token.kind.is_ident(), missing_ident_kind);

        if ident.name == keyword::IDENT_ARGUMENTS
            && self.parse_context.intersects(
                ParseContext::CLASS_FIELD_DEFINITION.union(ParseContext::CLASS_STATIC_BLOCK),
            )
        {
            let error =
                errors::ArgumentsCannotBeReferenced::new_in_property_initializer(ident.span);
            self.push_error(Box::new(error));
        }

        self.alloc(ast::Expr {
            kind: ast::ExprKind::Ident(ident),
        })
    }

    pub(super) fn parse_prop_name<const ALLOW_COMPUTED_PROP_NAMES: bool>(
        &mut self,
    ) -> &'cx ast::PropName<'cx> {
        let kind = match self.token.kind {
            TokenKind::String => {
                let raw = self.parse_string_lit();
                let key = self.string_key_value.unwrap();
                Some(ast::PropNameKind::StringLit { raw, key })
            }
            TokenKind::Number => {
                let lit = self.parse_num_lit(self.number_token(), false);
                Some(ast::PropNameKind::NumLit(lit))
            }
            TokenKind::BigInt => {
                let val = self.ident_token();
                let n = self.parse_bigint_lit(false, val);
                Some(ast::PropNameKind::BigIntLit(n))
            }
            _ => None,
        };
        if let Some(kind) = kind {
            (self.alloc(ast::PropName { kind })) as _
        } else if ALLOW_COMPUTED_PROP_NAMES && self.token.kind == TokenKind::LBracket {
            let start = self.token.start();
            self.expect(TokenKind::LBracket);
            let Ok(expr) = self.allow_in_and(Self::parse_expr) else {
                todo!("remove error result")
            };
            self.expect(TokenKind::RBracket);
            let span = self.new_span(start);
            let kind = self.create_computed_prop_name(span, expr);

            (self.alloc(ast::PropName {
                kind: ast::PropNameKind::Computed(kind),
            })) as _
        } else if self.token.kind == TokenKind::PrivateIdent {
            let ident = self.parse_private_ident();
            let kind = ast::PropNameKind::PrivateIdent(ident);
            self.alloc(ast::PropName { kind })
        } else {
            let ident = self.parse_ident_name();
            let kind = ast::PropNameKind::Ident(ident);
            self.alloc(ast::PropName { kind })
        }
    }

    fn parse_private_ident(&mut self) -> &'cx ast::PrivateIdent {
        debug_assert!(self.token.kind == TokenKind::PrivateIdent);
        let name = self.ident_token();
        let span = self.token.span;
        let id = self.next_node_id();
        let private_ident = self.alloc(ast::PrivateIdent { id, span, name });
        self.nodes
            .insert(id, ast::Node::PrivateIdent(private_ident));
        self.next_token();
        private_ident
    }

    #[inline(always)]
    pub(super) fn is_ident(&self) -> bool {
        let t = self.token.kind;
        if t == TokenKind::Ident {
            return true;
        }

        t.is_contextual_keyword() || t.is_strict_mode_reserved_word()
    }

    pub(super) fn parse_modifiers<
        const STOP_ON_START_OF_CLASS_STATIC_BLOCK: bool,
        const PERMIT_CONST_AS_MODIFIER: bool,
    >(
        &mut self,
        _allow_decorators: bool,
    ) -> Option<&'cx ast::Modifiers<'cx>> {
        let push_precede_error = |this: &mut Self, x: &ast::Modifier, y: ast::ModifierKind| {
            let error = errors::XModifierMustPrecedeYModifier {
                span: x.span(),
                x: x.kind(),
                y,
            };
            this.push_error(Box::new(error));
        };
        let push_already_seen_error = |this: &mut Self, x: &ast::Modifier| {
            let error = errors::ModifierAlreadySeen {
                span: x.span(),
                modifier: x.kind(),
            };
            this.push_error(Box::new(error));
        };
        let start = self.token.start();
        let mut list = Vec::with_capacity(4);
        let _has_leading_modifier = false;
        let _has_trailing_decorator = false;
        let mut flags = ast::ModifierFlags::empty();
        loop {
            let Some(m) = self
                .parse_modifier::<STOP_ON_START_OF_CLASS_STATIC_BLOCK, PERMIT_CONST_AS_MODIFIER>(
                    flags.contains(ast::ModifierFlags::STATIC),
                )
            else {
                break;
            };
            let modifier_kind = m.kind();
            let modifier_flag = modifier_kind.into_flag();

            match modifier_kind {
                ast::ModifierKind::Override => {
                    if flags.contains(ast::ModifierFlags::READONLY) {
                        push_precede_error(self, m, ast::ModifierKind::Readonly);
                    } else if flags.contains(ast::ModifierFlags::ACCESSOR) {
                        push_precede_error(self, m, ast::ModifierKind::Accessor);
                    } else if flags.contains(ast::ModifierFlags::ASYNC) {
                        push_precede_error(self, m, ast::ModifierKind::Async);
                    }
                }
                ast::ModifierKind::Public
                | ast::ModifierKind::Protected
                | ast::ModifierKind::Private => {
                    if flags.contains(ast::ModifierFlags::STATIC) {
                        push_precede_error(self, m, ast::ModifierKind::Static);
                    } else if flags.contains(ast::ModifierFlags::ACCESSOR) {
                        push_precede_error(self, m, ast::ModifierKind::Accessor);
                    } else if flags.contains(ast::ModifierFlags::READONLY) {
                        push_precede_error(self, m, ast::ModifierKind::Readonly);
                    } else if flags.contains(ast::ModifierFlags::ASYNC) {
                        push_precede_error(self, m, ast::ModifierKind::Async);
                    }
                }
                ast::ModifierKind::Static => {
                    if flags.contains(ast::ModifierFlags::READONLY) {
                        push_precede_error(self, m, ast::ModifierKind::Readonly);
                    } else if flags.contains(ast::ModifierFlags::ASYNC) {
                        push_precede_error(self, m, ast::ModifierKind::Async);
                    } else if flags.contains(ast::ModifierFlags::ACCESSOR) {
                        push_precede_error(self, m, ast::ModifierKind::Accessor);
                    } else if flags.contains(ast::ModifierFlags::OVERRIDE) {
                        push_precede_error(self, m, ast::ModifierKind::Override);
                    }
                }
                ast::ModifierKind::Export => {
                    if flags.contains(ast::ModifierFlags::EXPORT) {
                        push_already_seen_error(self, m);
                    }
                }
                _ => {}
            }

            flags.insert(modifier_flag);
            list.push(m);
        }
        if list.is_empty() {
            None
        } else {
            let span = self.new_span(start);
            let modifiers = self.alloc(list);
            Some(self.create_modifiers(span, modifiers, flags))
        }
    }

    pub(super) fn parse_ident_name(&mut self) -> &'cx ast::Ident {
        let is_ident = self.token.kind.is_ident_or_keyword();
        self.create_ident(is_ident, None)
    }

    pub(super) fn try_parse_semi(&mut self) -> bool {
        if !self.can_parse_semi() {
            false
        } else {
            if self.token.kind == TokenKind::Semi {
                self.next_token();
            }
            true
        }
    }

    #[inline(always)]
    pub(super) fn parse_semi_after_prop_name(&mut self, node_span: Span) {
        if self.try_parse_semi() {
            return;
        }
        self.parse_error_for_missing_semicolon_after(node_span);
    }

    fn parse_error_for_missing_semicolon_after(&mut self, node_span: Span) {
        let error = errors::UnexpectedKeywordOrIdentifier { span: node_span };
        self.push_error(Box::new(error));
    }

    pub(super) fn is_implements_clause(&mut self) -> bool {
        self.token.kind == TokenKind::Implements
            && self
                .lookahead(Lookahead::next_token_is_ident)
                .unwrap_or_default()
    }

    pub(super) fn is_index_sig(&mut self) -> bool {
        self.token.kind == TokenKind::LBracket
            && self
                .lookahead(|this| -> PResult<bool> {
                    this.p().next_token();

                    if this.p().token.kind == TokenKind::DotDotDot
                        || this.p().token.kind == TokenKind::RBracket
                    {
                        return Ok(true);
                    } else if this.p().token.kind.is_modifier_kind() {
                        this.p().next_token();
                        if this.p().is_ident() {
                            return Ok(true);
                        }
                    } else if !this.p().is_ident() {
                        return Ok(false);
                    } else {
                        this.p().next_token();
                    }

                    if this.p().token.kind == TokenKind::Colon
                        || this.p().token.kind == TokenKind::Comma
                    {
                        return Ok(true);
                    } else if this.p().token.kind != TokenKind::Question {
                        return Ok(false);
                    }

                    this.p().next_token();

                    Ok(this.p().token.kind == TokenKind::Colon
                        || this.p().token.kind == TokenKind::Comma
                        || this.p().token.kind == TokenKind::RBracket)
                })
                .unwrap_or_default()
    }

    pub(super) fn parse_params_worker(
        &mut self,
        flags: SignatureFlags,
        allow_ambiguity: bool,
    ) -> ast::ParamsDecl<'cx> {
        let saved_yield_context = self
            .node_context_flags
            .contains(ast::NodeFlags::YIELD_CONTEXT);
        let saved_await_context = self
            .node_context_flags
            .contains(ast::NodeFlags::AWAIT_CONTEXT);

        self.set_yield_context(flags.contains(SignatureFlags::YIELD));
        self.set_await_context(flags.contains(SignatureFlags::AWAIT));

        let old_error = self.diags.len();
        let params = self.parse_delimited_list::<false, _>(ParsingContext::PARAMETERS, |this| {
            if allow_ambiguity {
                Self::parse_param(this, allow_ambiguity)
            } else {
                Self::parse_param(this, false)
            }
        });
        let has_error = self.diags.len() > old_error;
        if !has_error {
            let last_is_rest = params
                .last()
                .map(|p| p.dotdotdot.is_some())
                .unwrap_or_default();
            if let Some(rest_param_but_not_last) = (!last_is_rest).then(|| {
                params
                    .iter()
                    .rev()
                    .skip(1)
                    .filter(|param| param.dotdotdot.is_some())
                    .rev()
                    .collect::<Vec<_>>()
            }) {
                for error_param in rest_param_but_not_last {
                    let error = bolt_ts_ecma_rules::ARestParameterMustBeLastInAParameterList {
                        span: error_param.span,
                    };
                    self.push_error(Box::new(error));
                }
            }
        }

        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        params
    }

    pub(super) fn parse_params(&mut self) -> ast::ParamsDecl<'cx> {
        use bolt_ts_ast::TokenKind::*;
        if !self.expect(LParen) {
            return &[];
        }
        let params = self.parse_params_worker(SignatureFlags::empty(), true);
        self.expect(RParen);
        params
    }

    pub(super) fn parse_binding_with_ident(
        &mut self,
        ident: Option<&'cx ast::Ident>,
    ) -> &'cx ast::Binding<'cx> {
        let ident = if let Some(ident) = ident {
            ident
        } else {
            self.create_ident(true, None)
        };
        self.create_binding(ast::BindingKind::Ident(ident))
    }

    pub(super) fn parse_param(
        &mut self,
        allow_ambiguity_name: bool,
    ) -> PResult<&'cx ast::ParamDecl<'cx>> {
        let start = self.token.start();
        let modifiers = self.parse_modifiers::<false, false>(false);
        const INVALID_MODIFIERS: ast::ModifierFlags =
            ast::ModifierFlags::STATIC.union(ast::ModifierFlags::EXPORT);
        if modifiers
            .map(|ms| ms.flags.intersects(INVALID_MODIFIERS))
            .unwrap_or_default()
            && let Some(ms) = modifiers.map(|ms| {
                ms.list
                    .iter()
                    .filter(|m| INVALID_MODIFIERS.contains(m.kind().into_flag()))
                    .copied()
            })
        {
            for m in ms {
                let error = errors::ModifierCannotAppearOnAParameter {
                    span: m.span(),
                    kind: m.kind(),
                };
                self.push_error(Box::new(error));
            }
        }

        if self.token.kind == TokenKind::This {
            let name = self.parse_binding_with_ident(None);
            let ty = self.parse_ty_anno()?;
            let id = self.next_node_id();
            let decl = self.alloc(ast::ParamDecl {
                id,
                span: self.new_span(start),
                modifiers,
                dotdotdot: None,
                name,
                question: None,
                ty,
                init: None,
            });
            self.nodes.insert(id, ast::Node::ParamDecl(decl));
            return Ok(decl);
        }

        let dotdotdot = self.parse_optional(TokenKind::DotDotDot).map(|t| t.span);
        if !allow_ambiguity_name
            && !(self.token.kind.is_binding_ident()
                || matches!(self.token.kind, TokenKind::LBracket | TokenKind::LBrace))
        {
            return Err(());
        }
        let name = self.parse_name_of_param()?;
        self.check_contextual_binding(name);
        if dotdotdot.is_some()
            && let Some(ms) = modifiers
            && ms.flags.intersects(ast::ModifierFlags::PARAMETER_PROPERTY)
        {
            let kinds = ms
                .list
                .iter()
                .filter_map(|m| {
                    if ast::ModifierFlags::PARAMETER_PROPERTY.contains(m.kind().into_flag()) {
                        Some(m.kind())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            let span = Span::new(ms.span.lo(), name.span.hi(), name.span.module());
            let error =
                errors::AParameterPropertyCannotBeDeclaredUsingARestParameter { span, kinds };
            self.push_error(Box::new(error));
        };
        let question = self.parse_optional(TokenKind::Question).map(|t| t.span);
        let ty = self.parse_ty_anno()?;
        let init = self.parse_init()?;
        let id = self.next_node_id();
        let decl = self.alloc(ast::ParamDecl {
            id,
            span: self.new_span(start),
            modifiers,
            dotdotdot,
            name,
            question,
            ty,
            init,
        });
        self.nodes.insert(id, ast::Node::ParamDecl(decl));
        Ok(decl)
    }

    pub(super) fn parse_ty_member_semi(&mut self) {
        if self.parse_optional(TokenKind::Comma).is_some() {
            return;
        }
        self.parse_semi();
    }

    pub(super) fn is_start_of_fn_or_ctor_ty(&mut self) -> bool {
        let skip_param_start = |this: &mut Self| -> PResult<bool> {
            if this.token.kind.is_modifier_kind() {
                this.parse_modifiers::<false, false>(false);
            }
            if this.token.kind.is_ident() || this.token.kind == TokenKind::This {
                this.next_token();
                Ok(true)
            } else if matches!(this.token.kind, TokenKind::LBracket | TokenKind::LBrace) {
                let len = this.diags.len();
                // todo: parse ident or pattern
                this.parse_ident(None);
                this.diags.truncate(len);
                Ok(true)
            } else {
                Ok(false)
            }
        };

        let t = self.token.kind;
        (t == TokenKind::Less)
            || (t == TokenKind::LParen
                && self.lookahead(|this| {
                    this.p().next_token();
                    let t = this.p().token.kind;
                    use bolt_ts_ast::TokenKind::*;
                    if matches!(t, TokenKind::RParen | TokenKind::DotDotDot) {
                        return true;
                    } else if skip_param_start(this.p()).unwrap_or_default() {
                        if matches!(this.p().token.kind, Colon | Comma | Question | Eq) {
                            return true;
                        } else if this.p().token.kind == RParen {
                            this.p().next_token();
                            if this.p().token.kind == EqGreat {
                                return true;
                            }
                        }
                    }
                    false
                }))
            || (t == TokenKind::New
                || t == TokenKind::Abstract
                    && self.lookahead(|this| {
                        this.p().next_token();
                        this.p().token.kind == TokenKind::New
                    }))
    }

    pub(super) fn parse_contextual_modifier(&mut self, t: TokenKind) -> bool {
        self.token.kind == t && self.try_parse(Lookahead::next_token_can_follow_modifier)
    }

    pub(super) fn parse_num_lit(&mut self, val: f64, neg: bool) -> &'cx ast::NumLit {
        let val = if neg { -val } else { val };
        let lit = self.create_numeric_literal(val, self.token.span);
        self.next_token();
        lit
    }

    pub(super) fn parse_string_lit(&mut self) -> &'cx ast::StringLit {
        let val = self.string_token();
        let lit = self.create_lit(val, self.token.span);
        self.nodes.insert(lit.id, ast::Node::StringLit(lit));
        self.next_token();
        lit
    }

    pub(super) fn parse_no_substitution_template_lit(
        &mut self,
    ) -> &'cx ast::NoSubstitutionTemplateLit {
        let val = self.string_token();
        let lit = self.create_lit(val, self.token.span);
        self.nodes
            .insert(lit.id, ast::Node::NoSubstitutionTemplateLit(lit));
        self.next_token();
        lit
    }

    pub(super) fn has_preceding_line_break(&self) -> bool {
        self.token_flags.contains(TokenFlags::PRECEDING_LINE_BREAK)
    }

    fn create_missing_ty(&mut self) -> &'cx ast::Ty<'cx> {
        let start = self.token.start();
        let ident = self.create_ident_by_atom(keyword::IDENT_EMPTY, self.token.span);
        let name = self.alloc(ast::EntityName {
            kind: ast::EntityNameKind::Ident(ident),
        });
        let id = self.next_node_id();
        let ty = self.alloc(ast::ReferTy {
            id,
            span: self.new_span(start),
            name,
            ty_args: None,
        });
        self.nodes.insert(id, ast::Node::ReferTy(ty));
        self.alloc(ast::Ty {
            kind: ast::TyKind::Refer(ty),
        })
    }

    pub(super) fn parse_index_sig_decl(
        &mut self,
        start: u32,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::IndexSigDecl<'cx>> {
        debug_assert!(self.token.kind == TokenKind::LBracket);
        self.next_token(); // consume '['
        let mut params = Vec::with_capacity(1);
        if self.is_list_element(ParsingContext::PARAMETERS, false) {
            if let Ok(param) = self.parse_param(true) {
                params.push(param);
            };

            if self.token.kind == TokenKind::Comma {
                let error = errors::AnIndexSignatureCannotHaveATrailingComma {
                    span: self.token.span,
                };
                self.push_error(Box::new(error));
                self.next_token();
            }
        }
        self.expect(TokenKind::RBracket);

        if params.len() != 1 {
            let span = if !params.is_empty() {
                params[0].span
            } else {
                Span::new(start, self.token.span.hi(), self.module_id)
            };
            let error = errors::AnIndexSignatureMustHaveExactlyOneParameter { span };
            self.push_error(Box::new(error));
        }

        if let Some(param) = params.first() {
            if param.modifiers.is_some() {
                let error = errors::AnIndexSignatureParameterCannotHaveAnAccessibilityModifier {
                    span: param.span,
                };
                self.push_error(Box::new(error));
            }

            if let Some(question) = param.question {
                let error =
                    errors::AnIndexSignatureParameterCannotHaveAQuestionMark { span: question };
                self.push_error(Box::new(error));
            }

            if let Some(init) = param.init {
                let error =
                    errors::AnIndexSignatureParameterCannotHaveAnInitializer { span: init.span() };
                self.push_error(Box::new(error));
            }

            if let Some(dotdotdot) = param.dotdotdot {
                let error = errors::AnIndexSignatureCannotHaveARestParameter { span: dotdotdot };
                self.push_error(Box::new(error));
            }
        }

        let (name, name_ty) = if let Some(param) = params.first() {
            (
                param.name,
                param.ty.unwrap_or_else(|| self.create_missing_ty()),
            )
        } else {
            let missing_ident = self.create_ident_by_atom(keyword::IDENT_EMPTY, self.token.span);
            let name = self.parse_binding_with_ident(Some(missing_ident));
            (name, self.create_missing_ty())
        };
        let ty = match self.parse_ty_anno()? {
            Some(ty) => ty,
            None => {
                let lo = self.token.span.lo();
                let span = Span::new(lo, lo + 1, self.module_id);
                let error = errors::AnIndexSignatureMustHaveATypeAnnotation { span };
                self.push_error(Box::new(error));
                self.create_missing_ty()
            }
        };
        self.parse_ty_member_semi();
        let span = self.new_span(start);
        let sig = self.create_index_sig_decl(span, modifiers, name, name_ty, ty);
        Ok(sig)
    }

    fn check_body_during_parse_accessor(
        &mut self,
        ambient: bool,
        body: &mut Option<&'cx ast::BlockStmt<'cx>>,
    ) {
        if ambient {
            if let Some(body) = body {
                let error =
                    errors::AnImplementationCannotBeDeclaredInAmbientContexts { span: body.span };
                self.push_error(Box::new(error));
            }
            *body = None;
        } else if body.is_none() {
            let error = errors::ExpectX {
                span: self.token.span,
                x: '{'.to_string(),
            };
            self.push_error(Box::new(error));
        }
    }

    pub(super) fn parse_getter_accessor_decl(
        &mut self,
        start: u32,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        ambient: bool,
        flags: SignatureFlags,
    ) -> PResult<&'cx ast::GetterDecl<'cx>> {
        let name = self.parse_prop_name::<false>();
        let _ty_params = self.parse_ty_params();
        if !self.parse_params().is_empty() {
            self.push_error(Box::new(errors::AGetAccessorCannotHaveParameters {
                span: name.span(),
            }));
        }
        // TODO: assert params.is_none
        let ty = self.parse_return_ty::<true, false>()?;
        let mut body = self.parse_fn_block_or_semi(flags);
        self.check_body_during_parse_accessor(ambient, &mut body);
        let id = self.next_node_id();
        let decl = self.alloc(ast::GetterDecl {
            id,
            modifiers,
            span: self.new_span(start),
            name,
            ty,
            body,
        });
        self.nodes.insert(id, ast::Node::GetterDecl(decl));
        Ok(decl)
    }

    pub(super) fn parse_setter_accessor_decl(
        &mut self,
        start: u32,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        ambient: bool,
        flags: SignatureFlags,
    ) -> PResult<&'cx ast::SetterDecl<'cx>> {
        let name = self.parse_prop_name::<false>();
        let _ty_params = self.parse_ty_params();
        let params = self.parse_params();
        let params = if params.is_empty() {
            self.push_error(Box::new(errors::ASetAccessorMustHaveExactlyOneParameter {
                span: name.span(),
            }));
            params
        } else if params.len() > 2 {
            self.push_error(Box::new(errors::ASetAccessorMustHaveExactlyOneParameter {
                span: name.span(),
            }));
            params.split_at(2).0
        } else {
            params
        };
        let _ty = self.parse_return_ty::<true, false>()?;
        let mut body = self.parse_fn_block_or_semi(flags);
        self.check_body_during_parse_accessor(ambient, &mut body);
        let id = self.next_node_id();
        debug_assert!(params.len() <= 2);
        let decl = self.alloc(ast::SetterDecl {
            id,
            span: self.new_span(start),
            modifiers,
            name,
            params,
            body,
        });
        self.nodes.insert(id, ast::Node::SetterDecl(decl));
        Ok(decl)
    }

    pub(super) fn is_heritage_clause_extends_or_implements_keyword(&mut self) -> bool {
        if matches!(self.token.kind, TokenKind::Extends | TokenKind::Implements) {
            self.lookahead(Lookahead::next_token_is_start_of_expr)
        } else {
            false
        }
    }
}

pub(super) fn is_declaration_filename(filename: &[u8]) -> bool {
    const SUFFIX: &[u8] = b".d.ts";
    filename.ends_with(SUFFIX)
}

pub fn parse_pseudo_bigint<'a>(s: &'a str) -> std::borrow::Cow<'a, str> {
    let s = s.trim();
    let (s, log2_base) = if let Some(rest) = s.strip_prefix("0b") {
        (rest.strip_suffix('n').unwrap_or(rest), 1u32)
    } else if let Some(rest) = s.strip_prefix("0o") {
        (rest.strip_suffix('n').unwrap_or(rest), 3u32)
    } else if let Some(rest) = s.strip_prefix("0x") {
        (rest.strip_suffix('n').unwrap_or(rest), 4u32)
    } else {
        // Decimal: omit trailing 'n'
        let rest = s.strip_suffix('n').unwrap_or(s);
        let rest = rest.trim_start_matches('0'); // skip leading zeros
        return if rest.is_empty() {
            std::borrow::Cow::Borrowed("0")
        } else {
            std::borrow::Cow::Borrowed(rest)
        };
    };

    // For binary, octal, hex: skip leading zeros in digits
    let digits = s.trim_start_matches('0');
    let digits = if digits.is_empty() { "0" } else { digits };

    let base = match log2_base {
        1 => 2,
        3 => 8,
        4 => 16,
        _ => unreachable!(),
    };

    // Big number: use a vector to store 16-bit LE "segments"
    let bits_needed = digits.len() as u32 * log2_base;
    let segments_len = ((bits_needed >> 4) + if bits_needed & 15 != 0 { 1 } else { 0 }) as usize;
    let mut segments = vec![0u16; segments_len];

    // Add each digit, one at a time, lowest digit last
    let chars: Vec<char> = digits.chars().collect();
    let start_idx = 0; // skip already-trimmed prefix
    let end_idx = chars.len();

    let mut bit_offset = 0u32;
    for i in (start_idx..end_idx).rev() {
        let c = chars[i];
        // Hex-digit to numeric value
        let digit = match c {
            '0'..='9' => (c as u8 - b'0') as u16,
            'a'..='f' => 10 + (c as u8 - b'a') as u16,
            'A'..='F' => 10 + (c as u8 - b'A') as u16,
            _ => unreachable!(),
        };
        let segment = (bit_offset >> 4) as usize;
        let shifted_digit = (digit as u32) << (bit_offset & 15);
        segments[segment] |= (shifted_digit & 0xFFFF) as u16;
        let residual = shifted_digit >> 16;
        if residual != 0 && segment + 1 < segments.len() {
            segments[segment + 1] |= residual as u16;
        }
        bit_offset += log2_base;
    }

    // Repeatedly divide segments by 10 and collect remainders for decimal string
    let mut base10_value = String::new();
    let mut first_nonzero_segment = segments.len().saturating_sub(1);
    let mut segments_remaining = true;
    while segments_remaining {
        let mut mod10 = 0u32;
        segments_remaining = false;
        for segment in (0..=first_nonzero_segment).rev() {
            let new_segment = (mod10 << 16) | segments[segment] as u32;
            let segment_value = new_segment / 10;
            segments[segment] = segment_value as u16;
            mod10 = new_segment - segment_value * 10;
            if segment_value != 0 && !segments_remaining {
                first_nonzero_segment = segment;
                segments_remaining = true;
            }
        }
        base10_value.insert(0, std::char::from_digit(mod10, 10).unwrap());
    }

    std::borrow::Cow::Owned(base10_value)
}
