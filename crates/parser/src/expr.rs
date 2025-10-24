use crate::parsing_ctx::{ParseContext, ParsingContext};

use super::lookahead::Lookahead;
use super::paren_rule::{NoParenRule, ParenRuleTrait};
use super::parse_fn_like::ParseFnExpr;
use super::state::LanguageVariant;
use super::{PResult, ParserState};
use super::{Tristate, parse_class_like};
use super::{errors, parsing_ctx};
use bolt_ts_ast::{self as ast, ModifierKind, NodeFlags, keyword};
use bolt_ts_ast::{BinPrec, Token, TokenKind};
use enumflags2::BitFlag;

impl<'cx> ParserState<'cx, '_> {
    fn is_update_expr(&self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        match self.token.kind {
            Plus | Minus | Tilde | Excl | Delete | Typeof | Void | Await => false,
            Less if !matches!(self.variant, LanguageVariant::Jsx) => false,
            _ => true,
        }
    }

    pub(super) fn parse_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let mut expr = self.parse_assign_expr_or_higher(false)?;

        while let Some(t) = self.parse_optional(TokenKind::Comma) {
            debug_assert_eq!(t.kind, TokenKind::Comma);
            let op = ast::BinOp {
                kind: t.kind.into(),
                span: t.span,
            };
            let right = self.parse_assign_expr_or_higher(false)?;
            let kind = ast::ExprKind::Bin(self.create_binary_expr(start, expr, op, right));
            expr = self.alloc(ast::Expr { kind });
        }
        Ok(expr)
    }

    pub(super) fn parse_init(&mut self) -> PResult<Option<&'cx ast::Expr<'cx>>> {
        if self.parse_optional(TokenKind::Eq).is_some() {
            self.parse_assign_expr_or_higher(false).map(Some)
        } else {
            Ok(None)
        }
    }

    fn try_parse_paren_arrow_fn_expr(&mut self) -> PResult<Option<&'cx ast::Expr<'cx>>> {
        match self.is_paren_arrow_fn_expr() {
            Tristate::True => self.parse_paren_arrow_fn_expr(false),
            Tristate::False => Ok(None),
            Tristate::Unknown => {
                self.try_parse(|this| this.p().parse_possible_paren_arrow_fn_expr())
            }
        }
    }

    fn parse_possible_paren_arrow_fn_expr(&mut self) -> PResult<Option<&'cx ast::Expr<'cx>>> {
        // let start = self.token.start();
        // TODO: cache

        self.parse_paren_arrow_fn_expr(false)
    }

    // TODO: put it into `parse_params`
    pub(super) fn check_params(
        &mut self,
        params: &'cx [&'cx ast::ParamDecl<'cx>],
        container_is_ctor_impl: bool,
    ) {
        for param in params {
            if let Some(mods) = param.modifiers {
                let mut flags = ModifierKind::empty();

                for m in mods.list {
                    if container_is_ctor_impl
                        && ModifierKind::ACCESSIBILITY.contains(m.kind)
                        && flags.intersects(ModifierKind::ACCESSIBILITY)
                    {
                        let error = errors::AccessibilityModifierAlreadySeen { span: m.span };
                        self.push_error(Box::new(error));
                    }
                    flags.insert(m.kind);
                }

                if !container_is_ctor_impl {
                    let error = Box::new(
                        errors::AParamPropIsOnlyAllowedInAConstructorImplementation {
                            span: mods.span,
                        },
                    );
                    self.push_error(error);
                }
            }
        }
    }

    fn parse_paren_arrow_fn_expr(
        &mut self,
        allow_ambiguity: bool,
    ) -> PResult<Option<&'cx ast::Expr<'cx>>> {
        let start = self.token.start();
        // TODO: mods
        // TODO: isAsync
        let ty_params = self.parse_ty_params();
        if self.token.kind != TokenKind::LParen {
            return Ok(None);
        }
        let params: ast::ParamsDecl<'cx>;

        if !self.expect(TokenKind::LParen) {
            if !allow_ambiguity {
                return Err(());
            }
            params = self.alloc([]);
        } else {
            params = self.parse_params_worker(allow_ambiguity);
            if !self.expect(TokenKind::RParen) && !allow_ambiguity {
                return Err(());
            }
        }

        self.check_params(params, false);

        // let has_ret_colon = self.token.kind == TokenKind::Colon;
        let ty = self.parse_ret_ty(true)?;
        if !allow_ambiguity
            && self.token.kind != TokenKind::EqGreat
            && self.token.kind != TokenKind::LBrace
        {
            return Err(());
        }
        let last_token = self.token.kind;
        self.expect(TokenKind::EqGreat);
        let body = if matches!(last_token, TokenKind::EqGreat | TokenKind::LBrace) {
            self.parse_arrow_fn_expr_body()?
        } else {
            todo!()
        };
        let id = self.next_node_id();
        let kind = self.alloc(ast::ArrowFnExpr {
            id,
            span: self.new_span(start),
            ty_params,
            params,
            ty,
            body,
        });
        self.nodes.insert(id, ast::Node::ArrowFnExpr(kind));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::ArrowFn(kind),
        });
        Ok(Some(expr))
    }

    fn parse_arrow_fn_expr_body(&mut self) -> PResult<ast::ArrowFnExprBody<'cx>> {
        if self.token.kind == TokenKind::LBrace {
            let block = self.parse_fn_block().unwrap();
            Ok(ast::ArrowFnExprBody::Block(block))
        } else {
            self.parse_assign_expr_or_higher(false)
                .map(ast::ArrowFnExprBody::Expr)
        }
    }

    fn parse_simple_arrow_fn_expr(
        &mut self,
        param: &'cx ast::Ident,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        debug_assert!(self.token.kind == TokenKind::EqGreat);
        let name = self.parse_binding_with_ident(Some(param));
        let param_id = self.next_node_id();
        let param = self.alloc(ast::ParamDecl {
            id: param_id,
            span: param.span,
            modifiers: None,
            dotdotdot: None,
            name,
            question: None,
            ty: None,
            init: None,
        });
        self.nodes.insert(param_id, ast::Node::ParamDecl(param));
        let params = self.alloc([param]);
        self.expect(TokenKind::EqGreat);
        let body = self.parse_arrow_fn_expr_body()?;
        let expr_id = self.next_node_id();
        let f = self.alloc(ast::ArrowFnExpr {
            id: expr_id,
            span: self.new_span(param.span.lo()),
            ty_params: None,
            params,
            ty: None,
            body,
        });
        self.nodes.insert(expr_id, ast::Node::ArrowFnExpr(f));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::ArrowFn(f),
        });
        Ok(expr)
    }

    pub(super) fn parse_assign_expr_or_higher(
        &mut self,
        allow_ret_ty_in_arrow_fn: bool,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        if let Ok(Some(expr)) = self.try_parse_paren_arrow_fn_expr() {
            return Ok(expr);
        };

        let start = self.token.start();
        let expr = self.parse_binary_expr(BinPrec::Lowest)?;
        if let ast::ExprKind::Ident(ident) = expr.kind
            && self.token.kind == TokenKind::EqGreat
        {
            return self.parse_simple_arrow_fn_expr(ident);
        }
        if expr.is_left_hand_side_expr_kind() && self.re_scan_greater().is_assignment() {
            // self.parent_map.r#override(expr.id(), id);
            let op = self.token.kind.into();
            self.parse_token_node();
            let right = self.parse_assign_expr_or_higher(false)?;
            let id = self.next_node_id();
            let expr = self.alloc(ast::AssignExpr {
                id,
                left: expr,
                op,
                right,
                span: self.new_span(start),
            });
            self.nodes.insert(id, ast::Node::AssignExpr(expr));
            let expr = self.alloc(ast::Expr {
                kind: ast::ExprKind::Assign(expr),
            });
            Ok(expr)
        } else {
            self.parse_cond_expr_rest(expr)
        }
    }

    fn parse_binary_expr(&mut self, prec: BinPrec) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let left = self.parse_unary_expr()?;
        self.parse_binary_expr_rest(prec, left, start)
    }

    fn parse_binary_expr_rest(
        &mut self,
        prec: BinPrec,
        left: &'cx ast::Expr<'_>,
        start: u32,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        let mut left = left;
        loop {
            self.re_scan_greater();

            let next_prec = self.token.kind.prec();
            let consume = if self.token.kind == TokenKind::AsteriskAsterisk {
                next_prec >= prec
            } else {
                next_prec > prec
            };
            if !consume {
                break Ok(left);
            }
            if self.token.kind == TokenKind::In && self.in_disallow_in_context() {
                break Ok(left);
            }

            if matches!(self.token.kind, TokenKind::As | TokenKind::Satisfies)
                && self.has_preceding_line_break()
            {
                break Ok(left);
            }

            let t = self.token;
            self.next_token();
            // self.parent_map.r#override(left.id(), next_expr_id);
            let kind = if matches!(t.kind, TokenKind::As | TokenKind::Satisfies) {
                let ty = self.parse_ty()?;
                if t.kind == TokenKind::Satisfies {
                    let next_expr_id = self.next_node_id();
                    let expr = self.alloc(ast::SatisfiesExpr {
                        id: next_expr_id,
                        span: self.new_span(start),
                        expr: left,
                        ty,
                    });
                    self.nodes
                        .insert(next_expr_id, ast::Node::SatisfiesExpr(expr));
                    ast::ExprKind::Satisfies(expr)
                } else {
                    let next_expr_id = self.next_node_id();
                    let expr = self.alloc(ast::AsExpr {
                        id: next_expr_id,
                        span: self.new_span(start),
                        expr: left,
                        ty,
                    });
                    self.nodes.insert(next_expr_id, ast::Node::AsExpr(expr));
                    ast::ExprKind::As(expr)
                }
            } else {
                let op = ast::BinOp {
                    kind: t.kind.into(),
                    span: t.span,
                };
                let right = self.parse_binary_expr(next_prec)?;
                let expr = self.create_binary_expr(start, left, op, right);
                ast::ExprKind::Bin(expr)
            };
            left = self.alloc(ast::Expr { kind });
        }
    }

    fn parse_unary_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        if self.is_update_expr() {
            self.parse_update_expr()
        } else {
            self.parse_simple_unary_expr()
        }
    }

    fn parse_prefix_unary_expr(
        &mut self,
        f: impl FnOnce(&mut Self) -> PResult<&'cx ast::Expr<'cx>>,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let op = self.token.kind.into();
        self.next_token();
        let expr = f(self)?;
        let id = self.next_node_id();
        let unary = self.alloc(ast::PrefixUnaryExpr {
            id,
            span: self.new_span(start),
            op,
            expr,
        });
        self.nodes.insert(id, ast::Node::PrefixUnaryExpr(unary));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::PrefixUnary(unary),
        });
        Ok(expr)
    }

    fn parse_simple_unary_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        match self.token.kind {
            Plus | Minus | Tilde | Excl => {
                self.parse_prefix_unary_expr(Self::parse_simple_unary_expr)
            }
            Typeof => self.parse_typeof_expr(),
            Void => self.parse_void_expr(),
            Less => {
                // TODO: is jsx
                self.parse_ty_assertion()
            }
            Delete => self.parse_delete_expr(),
            _ => self.parse_update_expr(),
        }
    }

    fn parse_delete_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        debug_assert!(self.token.kind == TokenKind::Delete);
        let start = self.token.start();
        self.next_token(); // consume `delete`
        let expr = self.parse_simple_unary_expr()?;
        let n = self.create_delete_expr(start, expr);
        let n = self.alloc(ast::Expr {
            kind: ast::ExprKind::Delete(n),
        });
        Ok(n)
    }

    fn parse_ty_assertion(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        debug_assert!(self.token.kind == TokenKind::Less);
        let start = self.token.start();
        self.next_token(); // consume `<`
        let ty = self.parse_ty()?;
        self.expect(TokenKind::Great);
        let expr = self.parse_simple_unary_expr()?;
        let id = self.next_node_id();
        let expr = self.alloc(ast::TyAssertion {
            id,
            span: self.new_span(start),
            ty,
            expr,
        });
        self.nodes.insert(id, ast::Node::TyAssertionExpr(expr));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::TyAssertion(expr),
        });
        Ok(expr)
    }

    fn parse_void_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Void);
        let expr = self.parse_expr()?;
        let id = self.next_node_id();
        let expr = self.alloc(ast::VoidExpr {
            id,
            span: self.new_span(start),
            expr,
        });
        self.nodes.insert(id, ast::Node::VoidExpr(expr));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Void(expr),
        });
        Ok(expr)
    }

    fn parse_typeof_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Typeof);
        let expr = self.parse_simple_unary_expr()?;
        let id = self.next_node_id();
        let kind = self.alloc(ast::TypeofExpr {
            id,
            span: self.new_span(start),
            expr,
        });
        self.nodes.insert(id, ast::Node::TypeofExpr(kind));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Typeof(kind),
        });
        Ok(expr)
    }

    fn parse_update_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        if matches!(self.token.kind, TokenKind::PlusPlus | TokenKind::MinusMinus) {
            self.parse_prefix_unary_expr(Self::parse_left_hand_side_expr_or_higher)
        } else if self.token.kind == TokenKind::Less
            && matches!(self.variant, LanguageVariant::Jsx)
            && self
                .lookahead(Lookahead::next_token_is_ident_or_keyword_or_great)
                .unwrap_or_default()
        {
            let jsx = self.parse_jsx_ele_or_self_closing_ele_or_frag(true, None, None, false)?;
            use super::jsx::JsxEleOrSelfClosingEleOrFrag::*;
            let kind = match jsx {
                Ele(n) => ast::ExprKind::JsxElem(n),
                SelfClosingEle(n) => ast::ExprKind::JsxSelfClosingElem(n),
                Frag(n) => ast::ExprKind::JsxFrag(n),
            };
            let expr = self.alloc(ast::Expr { kind });
            Ok(expr)
        } else {
            let start = self.token.start();
            let expr = self.parse_left_hand_side_expr_or_higher()?;
            // assert!(is_left_hand_side_expr_kind(expr));
            if matches!(self.token.kind, TokenKind::PlusPlus | TokenKind::MinusMinus)
                && !self.has_preceding_line_break()
            {
                let op = self.token.kind.into();
                self.next_token();
                let id = self.next_node_id();
                let unary = self.alloc(ast::PostfixUnaryExpr {
                    id,
                    span: self.new_span(start),
                    op,
                    expr,
                });
                self.nodes.insert(id, ast::Node::PostfixUnaryExpr(unary));
                let expr = self.alloc(ast::Expr {
                    kind: ast::ExprKind::PostfixUnary(unary),
                });
                Ok(expr)
            } else {
                Ok(expr)
            }
        }
    }

    pub fn parse_left_hand_side_expr_or_higher(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let expr = if self.token.kind == TokenKind::Import {
            todo!()
        } else if self.token.kind == TokenKind::Super {
            self.parse_super_expr()?
        } else {
            self.parse_member_expr_or_higher()?
        };
        self.parse_call_expr_rest(start as usize, expr)
    }

    fn parse_super_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let expr = self.make_super_expr();

        if self.token.kind == TokenKind::Less {
            let start_pos = self.token.start();
            if let Ok(Some(ty_args)) = self.try_parse(|l| l.p().parse_ty_args_in_expr()) {
                todo!("error handler")
            }
        }

        if matches!(
            self.token.kind,
            TokenKind::LParen | TokenKind::Dot | TokenKind::LBracket
        ) {
            Ok(expr)
        } else {
            self.expect(TokenKind::Dot);
            let name = self.parse_right_side_of_dot(true);
            let id = self.next_node_id();
            let expr = self.alloc(ast::PropAccessExpr {
                id,
                span: self.new_span(start),
                expr,
                question_dot: None,
                name,
            });
            self.nodes.insert(id, ast::Node::PropAccessExpr(expr));
            let expr = self.alloc(ast::Expr {
                kind: ast::ExprKind::PropAccess(expr),
            });
            Ok(expr)
        }
    }

    fn try_reparse_optional_chain(&mut self, mut n: &'cx ast::Expr<'cx>) -> bool {
        if self
            .node_flags_map
            .get(n.id())
            .intersects(ast::NodeFlags::OPTIONAL_CHAIN)
        {
            return true;
        }
        if let ast::ExprKind::NonNull(non_null) = n.kind {
            let mut expr = non_null.expr;
            loop {
                let ast::ExprKind::NonNull(non_null) = expr.kind else {
                    break;
                };
                if self
                    .node_flags_map
                    .get(expr.id())
                    .intersects(ast::NodeFlags::OPTIONAL_CHAIN)
                {
                    break;
                }
                expr = non_null.expr;
            }
            if self
                .node_flags_map
                .get(expr.id())
                .intersects(ast::NodeFlags::OPTIONAL_CHAIN)
            {
                while let ast::ExprKind::NonNull(non_null) = n.kind {
                    let flags = self
                        .node_flags_map
                        .0
                        .get_mut(&n.id().index_as_u32())
                        .unwrap();
                    *flags |= ast::NodeFlags::OPTIONAL_CHAIN;
                    n = non_null.expr;
                }

                return true;
            }
        }

        false
    }

    fn parse_call_expr_rest(
        &mut self,
        start: usize,
        mut expr: &'cx ast::Expr<'cx>,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        loop {
            expr = self.parse_member_expr_rest(start, expr, true)?;
            let create_call_expr = |this: &mut Self,
                                    e: &'cx ast::Expr<'cx>,
                                    ty_args: Option<&'cx ast::Tys<'cx>>,
                                    args: ast::Exprs<'cx>| {
                let id = this.next_node_id();
                let call = this.alloc(ast::CallExpr {
                    id,
                    span: this.new_span(start as u32),
                    ty_args,
                    expr: e,
                    args,
                });
                this.nodes.insert(id, ast::Node::CallExpr(call));
                this.alloc(ast::Expr {
                    kind: ast::ExprKind::Call(call),
                })
            };
            let mut ty_args = None;
            let question_dot = self.parse_optional(TokenKind::QuestionDot);
            if question_dot.is_some() {
                ty_args = self.try_parse(|l| l.p().parse_ty_args_in_expr())?;
                if self.is_template_start_of_tagged_template() {
                    expr = self.parse_tagged_template_rest(
                        start,
                        expr,
                        question_dot.map(|t| t.span),
                        ty_args,
                    )?;
                    continue;
                }
            }

            if ty_args.is_some() || self.token.kind == TokenKind::LParen {
                if question_dot.is_none()
                    && let ast::ExprKind::ExprWithTyArgs(expr_with_ty_args) = expr.kind
                {
                    ty_args = expr_with_ty_args.ty_args;
                    expr = expr_with_ty_args.expr;
                }
                let args = self.parse_args();
                if question_dot.is_some() || self.try_reparse_optional_chain(expr) {
                    todo!("call chain")
                } else {
                    expr = create_call_expr(self, expr, ty_args, args);
                };
                continue;
            }

            if question_dot.is_some() {
                // TODO:
            }

            break;
        }

        Ok(expr)
    }

    fn parse_args(&mut self) -> ast::Exprs<'cx> {
        self.expect(TokenKind::LParen);
        let args = self.parse_delimited_list::<false, _>(
            parsing_ctx::ParsingContext::ARGUMENT_EXPRESSIONS,
            Self::parse_arg,
        );
        self.expect(TokenKind::RParen);
        args
    }

    fn parse_arg(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.do_outside_of_parse_context(
            ParseContext::DISALLOW_IN_AND_DECORATOR,
            Self::parse_arg_or_array_lit_elem,
        )
    }

    fn parse_arg_or_array_lit_elem(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_assign_expr_or_higher(false)
    }

    fn parse_object_method_decl(
        &mut self,
        start: u32,
        name: &'cx ast::PropName<'cx>,
        asterisk_token: Option<Token>,
    ) -> PResult<&'cx ast::ObjectMember<'cx>> {
        // let is_generator = if asterisk_token.is_some() {
        //     todo!()
        // } else {

        // };
        let ty_params = self.parse_ty_params();
        let params = self.parse_params();
        self.check_params(params, false);
        let ty = self.parse_ty_anno()?;
        let body = self.parse_fn_block().unwrap();
        let node = self.create_object_method_member(start, name, ty_params, params, ty, body);
        let member = self.alloc(ast::ObjectMember {
            kind: ast::ObjectMemberKind::Method(node),
        });
        Ok(member)
    }

    fn parse_object_lit_member(&mut self) -> PResult<&'cx ast::ObjectMember<'cx>> {
        let start = self.token.start();
        if self.parse_optional(TokenKind::DotDotDot).is_some() {
            let expr = self.parse_assign_expr_or_higher(true)?;
            let id = self.next_node_id();
            let n = self.alloc(ast::SpreadAssignment {
                id,
                span: self.new_span(start),
                expr,
            });
            self.nodes.insert(id, ast::Node::SpreadAssignment(n));
            let m = self.alloc(ast::ObjectMember {
                kind: ast::ObjectMemberKind::SpreadAssignment(n),
            });
            return Ok(m);
        }

        let mods = self.parse_modifiers::<false, false>(false);

        let invalid_modifiers = |this: &mut Self| {
            if let Some(ms) = &mods {
                for m in ms.list {
                    if m.kind != ModifierKind::Async {
                        let error = errors::ModifierCannotBeUsedHere { span: m.span };
                        this.push_error(Box::new(error));
                    }
                }
            }
        };

        if self.parse_contextual_modifier(TokenKind::Get) {
            invalid_modifiers(self);
            let decl = self.parse_getter_accessor_decl(start, mods, false)?;
            return Ok(self.alloc(ast::ObjectMember {
                kind: ast::ObjectMemberKind::Getter(decl),
            }));
        } else if self.parse_contextual_modifier(TokenKind::Set) {
            invalid_modifiers(self);
            let decl = self.parse_setter_accessor_decl(start, mods, false)?;
            return Ok(self.alloc(ast::ObjectMember {
                kind: ast::ObjectMemberKind::Setter(decl),
            }));
        }

        let asterisk_token = self.parse_optional(TokenKind::Asterisk);
        let name = self.parse_prop_name(true);
        if let Some(question_token) = self.parse_optional(TokenKind::Question) {
            let error = errors::AnObjectMemberCannotBeDeclaredOptional {
                span: question_token.span,
            };
            self.push_error(Box::new(error));
        }
        let excl_token = self.parse_optional(TokenKind::Excl);
        if asterisk_token.is_some()
            || matches!(self.token.kind, TokenKind::LParen | TokenKind::Less)
        {
            invalid_modifiers(self);
            return self.parse_object_method_decl(start, name, asterisk_token);
        } else if let Some(name) = name.kind.as_ident()
            && self.token.kind != TokenKind::Colon
        {
            let id = self.next_node_id();
            let kind = self.alloc(ast::ObjectShorthandMember {
                id,
                span: self.new_span(start),
                name,
            });
            self.nodes
                .insert(id, ast::Node::ObjectShorthandMember(kind));
            let member = self.alloc(ast::ObjectMember {
                kind: ast::ObjectMemberKind::Shorthand(kind),
            });
            return Ok(member);
        }
        self.expect(TokenKind::Colon);
        let value = self.parse_assign_expr_or_higher(false)?;
        let id = self.next_node_id();
        let kind = self.alloc(ast::ObjectPropAssignment {
            id,
            span: self.new_span(start),
            name,
            init: value,
        });
        self.nodes.insert(id, ast::Node::ObjectPropAssignment(kind));
        let member = self.alloc(ast::ObjectMember {
            kind: ast::ObjectMemberKind::PropAssignment(kind),
        });
        Ok(member)
    }

    fn parse_paren_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LParen);
        let expr = self.parse_expr()?;
        self.expect(TokenKind::RParen);
        let id = self.next_node_id();
        let expr = self.alloc(ast::ParenExpr {
            id,
            span: self.new_span(start),
            expr,
        });
        self.nodes.insert(id, ast::Node::ParenExpr(expr));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Paren(expr),
        });
        Ok(expr)
    }

    fn parse_array_lit(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        let open = TokenKind::LBracket;
        let open_bracket_parsed = self.expect(open);
        let elems = self.parse_array_lit_elems();
        self.parse_expected_matching_brackets(
            open,
            TokenKind::RBracket,
            open_bracket_parsed,
            start as usize,
        );
        let id = self.next_node_id();
        let lit = self.alloc(ast::ArrayLit {
            id,
            span: self.new_span(start),
            elems,
        });
        self.nodes.insert(id, ast::Node::ArrayLit(lit));

        (self.alloc(ast::Expr {
            // id: expr_id,
            kind: ast::ExprKind::ArrayLit(lit),
        })) as _
    }

    fn parse_spread_element(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::DotDotDot);
        let expr = self.parse_assign_expr_or_higher(true)?;
        let id = self.next_node_id();
        let node = self.alloc(ast::SpreadElement {
            id,
            span: self.new_span(start),
            expr,
        });
        self.nodes.insert(id, ast::Node::SpreadElement(node));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::SpreadElement(node),
        });
        Ok(expr)
    }

    fn parse_array_lit_elems(&mut self) -> &'cx [&'cx ast::Expr<'cx>] {
        self.parse_delimited_list::<false, _>(
            parsing_ctx::ParsingContext::ARRAY_LITERAL_MEMBERS,
            |this| match this.token.kind {
                TokenKind::DotDotDot => this.parse_spread_element(),
                TokenKind::Comma => {
                    let id = this.next_node_id();
                    let expr = this.alloc(ast::OmitExpr {
                        id,
                        span: this.token.span,
                    });
                    this.nodes.insert(id, ast::Node::OmitExpr(expr));
                    let expr = this.alloc(ast::Expr {
                        kind: ast::ExprKind::Omit(expr),
                    });
                    Ok(expr)
                }
                _ => this.parse_assign_expr_or_higher(false),
            },
        )
    }

    fn parse_lit_expr(&mut self) -> &'cx ast::Expr<'cx> {
        use bolt_ts_ast::TokenKind::*;
        let kind = match self.token.kind {
            Number => {
                let val = self.number_token();
                let lit = self.parse_num_lit(val, false);
                ast::ExprKind::NumLit(lit)
            }
            BigInt => {
                let val = self.ident_token();
                let lit = self.create_lit((false, val), self.token.span);
                self.nodes.insert(lit.id, ast::Node::BigIntLit(lit));
                self.next_token();
                ast::ExprKind::BigIntLit(lit)
            }
            False | True => {
                let v = self.token.kind == True;
                let lit = self.create_lit(v, self.token.span);
                self.nodes.insert(lit.id, ast::Node::BoolLit(lit));
                self.next_token();
                ast::ExprKind::BoolLit(lit)
            }
            Null => {
                let lit = self.create_lit((), self.token.span);
                self.nodes.insert(lit.id, ast::Node::NullLit(lit));
                self.next_token();
                ast::ExprKind::NullLit(lit)
            }
            String => {
                let lit = self.parse_string_lit();
                ast::ExprKind::StringLit(lit)
            }
            NoSubstitutionTemplate => {
                let lit = self.parse_no_substitution_template_lit();
                ast::ExprKind::NoSubstitutionTemplateLit(lit)
            }
            This => {
                let this = self.create_this_expr(self.token.span);
                self.next_token();
                ast::ExprKind::This(this)
            }
            Regexp => {
                let id = self.next_node_id();
                let val = self.ident_token();
                let lit = self.alloc(ast::RegExpLit {
                    id,
                    span: self.token.span,
                    val,
                });
                self.next_token();
                self.nodes.insert(lit.id, ast::Node::RegExpLit(lit));
                ast::ExprKind::RegExpLit(lit)
            }
            _ => unreachable!(),
        };
        self.alloc(ast::Expr { kind })
    }

    fn make_super_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let span = self.token.span;
        self.expect(TokenKind::Super);
        let id = self.next_node_id();
        let node = self.alloc(ast::SuperExpr { id, span });
        self.nodes.insert(node.id, ast::Node::SuperExpr(node));

        (self.alloc(ast::Expr {
            kind: ast::ExprKind::Super(node),
        })) as _
    }

    fn parse_primary_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        match self.token.kind {
            NoSubstitutionTemplate | String | BigInt | Number | True | False | Null | This => {
                Ok(self.parse_lit_expr())
            }
            LBracket => Ok(self.parse_array_lit()),
            LParen => self.parse_paren_expr(),
            LBrace => Ok(self.parse_object_lit()),
            Function => self.parse_fn_expr(),
            New => self.parse_new_expr(),
            Class => self.parse_class_expr(),
            Super => Ok(self.make_super_expr()),
            TemplateHead => self.prase_template_expr(false),
            Slash | SlashEq => {
                self.re_scan_slash_token(false);
                if self.token.kind == TokenKind::Regexp {
                    Ok(self.parse_lit_expr())
                } else {
                    Ok(self.parse_ident(Some(errors::MissingIdentKind::ExpressionExpected)))
                }
            }
            _ => Ok(self.parse_ident(Some(errors::MissingIdentKind::ExpressionExpected))),
        }
    }

    fn prase_template_expr(&mut self, is_tagged_template: bool) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let head = self.parse_template_head(is_tagged_template)?;
        let spans = self.parse_template_spans(|this| {
            this.parse_template_span(is_tagged_template)
                .map(|n| (n, !n.is_tail))
        })?;
        let id = self.next_node_id();
        let n = self.alloc(ast::TemplateExpr {
            id,
            span: self.new_span(start),
            head,
            spans,
        });
        self.nodes.insert(n.id, ast::Node::TemplateExpr(n));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Template(n),
        });
        Ok(expr)
    }

    pub(super) fn parse_template_head(
        &mut self,
        is_tagged_template: bool,
    ) -> PResult<&'cx ast::TemplateHead> {
        if !is_tagged_template {
            // self.re_scan_greater()
        }
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateHead {
            id,
            span: self.token.span,
            text: self.token_value.unwrap().ident(),
        });
        self.nodes.insert(node.id, ast::Node::TemplateHead(node));
        self.next_token();
        Ok(node)
    }

    pub(super) fn parse_template_spans<T>(
        &mut self,
        f: impl Fn(&mut Self) -> PResult<(&'cx T, bool)>,
    ) -> PResult<&'cx [&'cx T]> {
        let mut spans = Vec::with_capacity(8);
        loop {
            let (node, is_template_middle) = f(self)?;
            spans.push(node);
            if !is_template_middle {
                break;
            }
        }
        Ok(self.alloc(spans))
    }

    pub(super) fn parse_template_span_text(
        &mut self,
        is_tagged_template: bool,
    ) -> (bolt_ts_atom::Atom, bool) {
        if self.token.kind == TokenKind::RBrace {
            self.re_scan_template_token(is_tagged_template);
            let atom = self.token_value.unwrap().ident();
            let is_tail = self.token.kind == TokenKind::TemplateTail;
            self.next_token();
            (atom, is_tail)
        } else {
            self.expect_with::<true>(
                TokenKind::TemplateTail,
                Some(|this: &mut Self| {
                    Box::new(errors::ExpectX {
                        span: this.token.span,
                        x: "}".to_string(),
                    }) as _
                }),
            );
            (keyword::IDENT_EMPTY, true)
        }
    }

    fn parse_template_span(
        &mut self,
        is_tagged_template: bool,
    ) -> PResult<&'cx ast::TemplateSpan<'cx>> {
        let start = self.token.start();
        let expr = self.allow_in_and(Self::parse_expr)?;
        let (text, is_tail) = self.parse_template_span_text(is_tagged_template);
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateSpan {
            id,
            span: self.new_span(start),
            expr,
            text,
            is_tail,
        });
        self.nodes.insert(node.id, ast::Node::TemplateSpan(node));
        Ok(node)
    }

    fn parse_class_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let kind = self.parse_class_decl_or_expr(parse_class_like::ParseClassExpr, None)?;
        Ok(self.alloc(ast::Expr {
            kind: ast::ExprKind::Class(kind),
        }))
    }

    fn parse_new_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        let start = self.token.start();
        self.expect(New);
        let expr = self.parse_primary_expr()?;
        let mut expr = self.parse_member_expr_rest(start as usize, expr, false)?;
        let mut ty_args = None;
        if let ast::ExprKind::ExprWithTyArgs(e) = expr.kind {
            ty_args = e.ty_args;
            expr = e.expr;
        }
        let args = if self.token.kind == LParen {
            Some(self.parse_args())
        } else {
            None
        };
        let id = self.next_node_id();
        let new = self.alloc(ast::NewExpr {
            id,
            span: self.new_span(start),
            expr,
            ty_args,
            args,
        });
        self.nodes.insert(id, ast::Node::NewExpr(new));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::New(new),
        });
        Ok(expr)
    }

    fn parse_fn_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let f = self.parse_fn_decl_or_expr(ParseFnExpr, None)?;
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Fn(f),
        });
        Ok(expr)
    }

    fn parse_object_lit(&mut self) -> &'cx ast::Expr<'cx> {
        use bolt_ts_ast::TokenKind::*;
        let start = self.token.start();
        let open = LBrace;
        let open_brace_parsed = self.expect(LBrace);
        let props = self.parse_delimited_list::<true, _>(
            parsing_ctx::ParsingContext::OBJECT_LITERAL_MEMBERS,
            Self::parse_object_lit_member,
        );
        let close = RBrace;
        self.parse_expected_matching_brackets(open, close, open_brace_parsed, start as usize);
        let id = self.next_node_id();
        let lit = self.alloc(ast::ObjectLit {
            id,
            span: self.new_span(start),
            members: props,
        });
        self.nodes.insert(id, ast::Node::ObjectLit(lit));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::ObjectLit(lit),
        });
        expr
    }

    fn parse_cond_expr_rest(&mut self, cond: &'cx ast::Expr<'cx>) -> PResult<&'cx ast::Expr<'cx>> {
        if self.parse_optional(TokenKind::Question).is_some() {
            let start = cond.span().lo();
            let when_true = self.parse_expr()?;
            self.expect(TokenKind::Colon);
            let when_false = self.parse_expr()?;
            let id = self.next_node_id();
            let expr = self.alloc(ast::CondExpr {
                id,
                span: self.new_span(start),
                cond,
                when_false,
                when_true,
            });
            self.nodes.insert(id, ast::Node::CondExpr(expr));
            let expr = self.alloc(ast::Expr {
                kind: ast::ExprKind::Cond(expr),
            });
            Ok(expr)
        } else {
            Ok(cond)
        }
    }

    fn parse_member_expr_or_higher(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let expr = self.parse_primary_expr()?;
        self.parse_member_expr_rest(start as usize, expr, true)
    }

    fn parse_prop_access_expr_rest(
        &mut self,
        start: usize,
        expr: &'cx ast::Expr<'cx>,
        question_dot: Option<bolt_ts_span::Span>,
    ) -> PResult<&'cx ast::PropAccessExpr<'cx>> {
        let name = self.parse_right_side_of_dot(true);
        let is_optional_chain = question_dot.is_some() || self.try_reparse_optional_chain(expr);
        let prop = if is_optional_chain {
            self.create_prop_access_chain(start as u32, expr, question_dot, name)
        } else {
            self.create_prop_access_expr(start as u32, expr, name)
        };
        Ok(prop)
    }

    fn parse_ele_access_expr_rest(
        &mut self,
        start: usize,
        expr: &'cx ast::Expr<'cx>,
        question_dot: Option<bolt_ts_span::Span>,
    ) -> PResult<&'cx ast::EleAccessExpr<'cx>> {
        let arg = if self.token.kind == TokenKind::RBracket {
            let error = errors::AnElementAccessExpressionShouldTakeAnArgument {
                span: self.token.span,
            };
            self.push_error(Box::new(error));
            let ident = self.create_ident_by_atom(keyword::IDENT_EMPTY, self.token.span);
            self.alloc(ast::Expr {
                kind: ast::ExprKind::Ident(ident),
            })
        } else {
            self.parse_expr()?
        };
        self.expect(TokenKind::RBracket);
        let ele = if question_dot.is_some() || self.try_reparse_optional_chain(expr) {
            todo!()
        } else {
            let expr = NoParenRule.paren_left_side_of_access(expr, false);
            let id = self.next_node_id();
            self.alloc(ast::EleAccessExpr {
                id,
                span: self.new_span(start as u32),
                expr,
                arg,
            })
        };
        self.nodes.insert(ele.id, ast::Node::EleAccessExpr(ele));
        Ok(ele)
    }

    pub(super) fn is_template_start_of_tagged_template(&self) -> bool {
        matches!(
            self.token.kind,
            TokenKind::NoSubstitutionTemplate | TokenKind::TemplateHead
        )
    }

    fn is_start_of_optional_prop_or_element_access_chain(&mut self) -> bool {
        self.token.kind == TokenKind::QuestionDot
            && self
                .lookahead(Lookahead::next_token_is_ident_or_keyword_or_open_bracket_or_template)
                .unwrap_or_default()
    }

    fn parse_tagged_template_rest(
        &mut self,
        start: usize,
        tag: &'cx ast::Expr<'cx>,
        question_dot: Option<bolt_ts_span::Span>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        let tpl = if self.token.kind == TokenKind::NoSubstitutionTemplate {
            self.re_scan_template_token(true);
            let lit = self.parse_no_substitution_template_lit();
            self.alloc(ast::Expr {
                kind: ast::ExprKind::NoSubstitutionTemplateLit(lit),
            })
        } else {
            self.prase_template_expr(true)?
        };
        let expr = self.create_tagged_template_expr(start as u32, tag, ty_args, tpl, question_dot);
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::TaggedTemplate(expr),
        });
        Ok(expr)
    }

    fn parse_member_expr_rest(
        &mut self,
        start: usize,
        mut expr: &'cx ast::Expr<'cx>,
        allow_optional_chain: bool,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        loop {
            let mut question_dot_token = None;
            let mut is_property_access = false;

            if allow_optional_chain && self.is_start_of_optional_prop_or_element_access_chain() {
                let old = self.token.span;
                self.expect(TokenKind::QuestionDot);
                question_dot_token = Some(old);
                is_property_access = self.token.kind.is_ident_or_keyword();
            } else {
                is_property_access = self.parse_optional(TokenKind::Dot).is_some();
            }

            if is_property_access {
                let prop = self.parse_prop_access_expr_rest(start, expr, question_dot_token)?;
                expr = self.alloc(ast::Expr {
                    kind: ast::ExprKind::PropAccess(prop),
                });
                continue;
            }

            if self.parse_optional(TokenKind::LBracket).is_some() {
                let ele = self.parse_ele_access_expr_rest(start, expr, question_dot_token)?;
                expr = self.alloc(ast::Expr {
                    kind: ast::ExprKind::EleAccess(ele),
                });
                continue;
            }

            // TODO: decorator

            if self.is_template_start_of_tagged_template() {
                if question_dot_token.is_none()
                    && let ast::ExprKind::ExprWithTyArgs(e) = expr.kind
                {
                    expr = self.parse_tagged_template_rest(
                        start,
                        e.expr,
                        question_dot_token,
                        e.ty_args,
                    )?;
                    continue;
                }
                expr = self.parse_tagged_template_rest(start, expr, question_dot_token, None)?;
                continue;
            }

            if question_dot_token.is_none() {
                if self.token.kind == TokenKind::Excl && !self.has_preceding_line_break() {
                    self.next_token();
                    let id = self.next_node_id();
                    let ele = self.alloc(ast::NonNullExpr {
                        id,
                        span: self.new_span(start as u32),
                        expr,
                    });
                    self.nodes.insert(id, ast::Node::NonNullExpr(ele));
                    expr = self.alloc(ast::Expr {
                        kind: ast::ExprKind::NonNull(ele),
                    });
                    continue;
                }
                if let Some(ty_args) = self.try_parse(|l| l.p().parse_ty_args_in_expr())? {
                    let id = self.next_node_id();
                    let ele = self.alloc(ast::ExprWithTyArgs {
                        id,
                        span: self.new_span(start as u32),
                        expr,
                        ty_args: Some(ty_args),
                    });
                    self.nodes.insert(id, ast::Node::ExprWithTyArgs(ele));
                    expr = self.alloc(ast::Expr {
                        kind: ast::ExprKind::ExprWithTyArgs(ele),
                    });
                    continue;
                }
            }

            return Ok(expr);
        }
    }

    pub(super) fn parse_ty_args_in_expr(&mut self) -> PResult<Option<&'cx ast::Tys<'cx>>> {
        let start = self.token.start();
        if self.re_scan_less() != TokenKind::Less {
            return Ok(None);
        }
        self.next_token();
        let list =
            self.parse_delimited_list::<false, _>(ParsingContext::TYPE_ARGUMENTS, Self::parse_ty);
        if list.is_empty() {
            let span = self.new_span(start);
            let error = errors::TypeArgumentListCannotBeEmpty { span };
            self.push_error(Box::new(error));
        }
        if self.re_scan_greater() != TokenKind::Great {
            return Ok(None);
        }
        self.next_token();
        if self.can_follow_ty_args_in_expr() {
            let ty_args = self.alloc(ast::Tys {
                span: self.new_span(start),
                list,
            });
            Ok(Some(ty_args))
        } else {
            Ok(None)
        }
    }

    fn can_follow_ty_args_in_expr(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        if matches!(
            self.token.kind,
            LParen | TemplateHead | NoSubstitutionTemplate
        ) {
            true
        } else if matches!(self.token.kind, Less | Great | Plus | Minus) {
            false
        } else {
            self.has_preceding_line_break() || !self.is_start_of_expr()
        }
    }
}
