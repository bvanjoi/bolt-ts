use super::ast::{self, BinOp};
use super::paren_rule::{NoParenRule, ParenRuleTrait};
use super::parse_fn_like::ParseFnExpr;
use super::token::{BinPrec, TokenKind};
use super::ty::TypeArguments;
use super::utils::is_left_hand_side_expr_kind;
use super::{errors, list_ctx};
use super::{parse_class_like, Tristate};
use super::{PResult, ParserState};

impl<'cx, 'p> ParserState<'cx, 'p> {
    fn is_update_expr(&self) -> bool {
        use TokenKind::*;
        match self.token.kind {
            Plus | Minus => false,
            Less => {
                // TODO: is jsx
                true
            }
            _ => true,
        }
    }

    pub(super) fn parse_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_assign_expr()
    }

    pub(super) fn parse_init(&mut self) -> Option<&'cx ast::Expr<'cx>> {
        self.parse_optional(TokenKind::Eq)
            .map(|_| self.parse_assign_expr().unwrap())
    }

    fn try_parse_paren_arrow_fn_expr(&mut self) -> PResult<Option<&'cx ast::Expr<'cx>>> {
        match self.is_paren_arrow_fn_expr() {
            Tristate::True => self.parse_paren_arrow_fn_expr().map(|expr| Some(expr)),
            Tristate::False => Ok(None),
            Tristate::Unknown => {
                // todo
                Ok(None)
            }
        }
    }

    fn parse_paren_arrow_fn_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let id = self.next_node_id();
        // TODO: mods
        // TODO: isAsync
        let ty_params = self.with_parent(id, Self::parse_ty_params)?;
        let params = self.with_parent(id, Self::parse_params)?;
        for param in params {
            if let Some(mods) = param.modifiers {
                let error = Box::new(
                    errors::AParamPropIsOnlyAllowedInAConstructorImplementation { span: mods.span },
                );
                self.push_error(error);
            }
        }
        let has_ret_colon = self.token.kind == TokenKind::Colon;
        let ty = self.with_parent(id, |this| this.parse_ret_ty(true))?;
        let last_token = self.token.kind;
        self.expect(TokenKind::EqGreat)?;
        let body = if matches!(last_token, TokenKind::EqGreat | TokenKind::LBrace) {
            self.parse_arrow_fn_expr_body()?
        } else {
            todo!()
        };
        let kind = self.alloc(ast::ArrowFnExpr {
            id,
            span: self.new_span(start),
            ty_params,
            params,
            ty,
            body,
        });
        self.insert_map(id, ast::Node::ArrowFnExpr(kind));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::ArrowFn(kind),
        });
        Ok(expr)
    }

    fn parse_arrow_fn_expr_body(&mut self) -> PResult<ast::ArrowFnExprBody<'cx>> {
        if self.token.kind == TokenKind::LBrace {
            self.parse_fn_block()
                .map(|block| ast::ArrowFnExprBody::Block(block.unwrap()))
        } else {
            self.parse_assign_expr()
                .map(|expr| ast::ArrowFnExprBody::Expr(expr))
        }
    }

    fn parse_simple_arrow_fn_expr(
        &mut self,
        param: &'cx ast::Ident,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        let expr_id = self.next_node_id();
        let param_id = self.next_node_id();
        let param = self.alloc(ast::ParamDecl {
            id: param_id,
            span: param.span,
            modifiers: None,
            dotdotdot: None,
            name: param,
            question: None,
            ty: None,
            init: None,
        });
        // self.parent_map.r#override(param.id, param_id);
        self.with_parent(expr_id, |this| {
            this.insert_map(param_id, ast::Node::ParamDecl(param));
        });
        let params = self.alloc([param]);
        self.expect(TokenKind::EqGreat)?;
        let body = self.parse_arrow_fn_expr_body()?;
        let f = self.alloc(ast::ArrowFnExpr {
            id: expr_id,
            span: self.new_span(param.span.lo),
            ty_params: None,
            params,
            ty: None,
            body,
        });
        self.insert_map(f.id, ast::Node::ArrowFnExpr(f));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::ArrowFn(f),
        });
        Ok(expr)
    }

    fn parse_assign_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        if let Ok(Some(expr)) = self.try_parse_paren_arrow_fn_expr() {
            return Ok(expr);
        };

        let start = self.token.start();
        let expr = self.parse_binary_expr(BinPrec::Lowest)?;
        if let ast::ExprKind::Ident(ident) = expr.kind {
            if self.token.kind == TokenKind::EqGreat {
                return self.parse_simple_arrow_fn_expr(ident);
            }
        }
        if is_left_hand_side_expr_kind(expr) && self.re_scan_greater().is_assignment() {
            let id = self.next_node_id();
            self.parent_map.r#override(expr.id(), id);
            let op = self.token.kind.into();
            self.parse_token_node();
            let right = self.with_parent(id, Self::parse_assign_expr)?;
            let expr = self.alloc(ast::AssignExpr {
                id,
                left: expr,
                op,
                right,
                span: self.new_span(start),
            });
            self.insert_map(id, ast::Node::AssignExpr(expr));
            let expr = self.alloc(ast::Expr {
                kind: ast::ExprKind::Assign(&expr),
            });
            Ok(expr)
        } else {
            self.parse_cond_expr_rest(expr)
        }
    }

    fn parse_binary_expr(&mut self, prec: BinPrec) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start() as usize;
        let left = self.parse_unary_expr()?;
        self.parse_binary_expr_rest(prec, left, start)
    }

    fn parse_binary_expr_rest(
        &mut self,
        prec: BinPrec,
        left: &'cx ast::Expr<'_>,
        start: usize,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        let mut left = left;
        loop {
            self.re_scan_greater();

            let next_prec = self.token.kind.prec();
            if !(next_prec > prec) {
                break Ok(left);
            }
            let op = BinOp {
                kind: self.token.kind.into(),
                span: self.token.span,
            };
            let bin_expr_id = self.next_node_id();
            self.next_token();
            self.parent_map.r#override(left.id(), bin_expr_id);
            let right = self.with_parent(bin_expr_id, |this| this.parse_binary_expr(next_prec))?;
            let bin_expr = self.alloc(ast::BinExpr {
                id: bin_expr_id,
                left,
                op,
                right,
                span: self.new_span(start as u32),
            });
            self.insert_map(bin_expr_id, ast::Node::BinExpr(bin_expr));
            let kind = ast::ExprKind::Bin(bin_expr);
            left = self.alloc(ast::Expr { kind });
        }
    }

    fn parse_unary_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        if self.is_update_expr() {
            // let start = self.token.start();
            self.parse_update_expr()
        } else {
            self.parse_simple_unary_expr()
        }
    }

    fn parse_prefix_unary_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let id = self.next_node_id();
        let op = self.token.kind.into();
        self.next_token();
        let expr = self.with_parent(id, Self::parse_simple_unary_expr)?;
        let unary = self.alloc(ast::PrefixUnaryExpr {
            id,
            span: self.new_span(start),
            op,
            expr,
        });
        self.insert_map(id, ast::Node::PrefixUnaryExpr(unary));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::PrefixUnary(unary),
        });
        Ok(expr)
    }

    fn parse_simple_unary_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        match self.token.kind {
            Plus | Minus => self.parse_prefix_unary_expr(),
            _ => self.parse_update_expr(),
        }
    }

    fn parse_update_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        if matches!(self.token.kind, TokenKind::PlusPlus | TokenKind::MinusMinus) {
            let id = self.next_node_id();
            let op = self.token.kind.into();
            self.next_token();
            let expr = self.parse_left_hand_side_expr();
            let unary = self.alloc(ast::PrefixUnaryExpr {
                id,
                span: self.new_span(start),
                op,
                expr,
            });
            self.insert_map(id, ast::Node::PrefixUnaryExpr(unary));
            let expr = self.alloc(ast::Expr {
                kind: ast::ExprKind::PrefixUnary(unary),
            });
            Ok(expr)
        } else {
            let expr = self.parse_left_hand_side_expr();
            self.parse_call_expr(start as usize, expr)
        }
    }

    fn parse_call_expr(
        &mut self,
        start: usize,
        mut expr: &'cx ast::Expr<'cx>,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        loop {
            expr = self.parse_member_expr_rest(start, expr)?;
            let parse_rest =
                |this: &mut Self, id: ast::NodeID, ty_args: Option<&'cx ast::Tys<'cx>>| {
                    let args = this.parse_args()?;
                    let call = this.alloc(ast::CallExpr {
                        id,
                        span: this.new_span(start as u32),
                        ty_args,
                        expr,
                        args,
                    });
                    this.insert_map(id, ast::Node::CallExpr(call));
                    Ok(this.alloc(ast::Expr {
                        kind: ast::ExprKind::Call(call),
                    }))
                };
            if self.token.kind == TokenKind::Less {
                let id = self.next_node_id();
                self.parent_map.r#override(expr.id(), id);
                let ty_args = self.parse_ty_args_in_expr()?;
                if let Some(ty_args) = ty_args {
                    if ty_args.list.is_empty() {
                        let error = errors::TypeArgumentListCannotBeEmpty { span: ty_args.span };
                        self.push_error(Box::new(error));
                    }
                }
                expr = parse_rest(self, id, ty_args)?;
            } else if self.token.kind == TokenKind::LParen {
                let id = self.next_node_id();
                self.parent_map.r#override(expr.id(), id);
                expr = parse_rest(self, id, None)?;
            } else {
                break Ok(expr);
            }
        }
    }

    fn parse_args(&mut self) -> PResult<ast::Exprs<'cx>> {
        self.expect(TokenKind::LParen)?;
        let args = self.parse_delimited_list(list_ctx::ArgExprs, Self::parse_arg);
        self.expect(TokenKind::RParen)?;
        Ok(args)
    }

    fn parse_arg(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_arg_or_array_lit_elem()
    }

    fn parse_arg_or_array_lit_elem(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_assign_expr()
    }

    fn parse_object_lit_ele(&mut self) -> PResult<&'cx ast::ObjectMemberField<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        // let mods = self.with_parent(id, Self::parse_modifiers)?;
        // let is_ident = self.is_ident();
        let name = self.with_parent(id, Self::parse_prop_name)?;
        self.parse_optional(TokenKind::Question);
        self.expect(TokenKind::Colon)?;
        let value = self.with_parent(id, Self::parse_assign_expr)?;
        let filed = self.alloc(ast::ObjectMemberField {
            id,
            span: self.new_span(start),
            name,
            value,
        });
        self.insert_map(id, ast::Node::ObjectMemberField(filed));
        Ok(filed)
    }

    fn parse_paren_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let id = self.next_node_id();
        self.expect(TokenKind::LParen)?;
        let expr = self.with_parent(id, Self::parse_expr)?;
        self.expect(TokenKind::RParen)?;
        let expr = self.alloc(ast::ParenExpr {
            id,
            span: self.new_span(start),
            expr,
        });
        self.insert_map(id, ast::Node::ParenExpr(expr));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Paren(expr),
        });
        Ok(expr)
    }

    fn parse_array_lit(&mut self) -> &'cx ast::Expr<'cx> {
        let id = self.next_node_id();
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
            span: self.new_span(start),
            elems,
        });
        self.insert_map(id, ast::Node::ArrayLit(&lit));
        let expr = self.alloc(ast::Expr {
            // id: expr_id,
            kind: ast::ExprKind::ArrayLit(lit),
        });
        expr
    }

    fn parse_array_lit_elems(&mut self) -> &'cx [&'cx ast::Expr<'cx>] {
        self.parse_delimited_list(list_ctx::ArrayLiteralMembers, |this| {
            if this.token.kind == TokenKind::Comma {
                let id = this.next_node_id();
                let expr = this.alloc(ast::OmitExpr {
                    id,
                    span: this.token.span,
                });
                this.insert_map(id, ast::Node::OmitExpr(expr));
                let expr = this.alloc(ast::Expr {
                    kind: ast::ExprKind::Omit(expr),
                });
                Ok(expr)
            } else {
                this.parse_assign_expr()
            }
        })
    }

    fn parse_lit_expr(&mut self) -> &'cx ast::Expr<'cx> {
        use TokenKind::*;
        let kind = match self.token.kind {
            Number => {
                let val = self.number_token();
                let lit = self.parse_num_lit(val, false);
                ast::ExprKind::NumLit(lit)
            }
            False | True => {
                let v = self.token.kind == True;
                let lit = self.create_lit(v, self.token.span);
                self.insert_map(lit.id, ast::Node::BoolLit(lit));
                self.next_token();
                ast::ExprKind::BoolLit(lit)
            }
            Null => {
                let lit = self.create_lit((), self.token.span);
                self.insert_map(lit.id, ast::Node::NullLit(lit));
                self.next_token();
                ast::ExprKind::NullLit(lit)
            }
            String | NoSubstitutionTemplate => {
                let lit = self.parse_string_lit();
                ast::ExprKind::StringLit(lit)
            }
            This => {
                let id = self.next_node_id();
                let this = self.alloc(ast::ThisExpr {
                    id,
                    span: self.token.span,
                });
                self.next_token();
                self.insert_map(this.id, ast::Node::ThisExpr(this));
                ast::ExprKind::This(this)
            }
            _ => unreachable!(),
        };
        self.alloc(ast::Expr { kind })
    }

    fn parse_primary_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        match self.token.kind {
            NoSubstitutionTemplate | String | Number | True | False | Null | This => {
                Ok(self.parse_lit_expr())
            }
            LBracket => Ok(self.parse_array_lit()),
            LParen => self.parse_paren_expr(),
            LBrace => self.parse_object_lit(),
            Function => self.parse_fn_expr(),
            New => self.parse_new_expr(),
            Class => self.parse_class_expr(),
            _ => Ok(self.parse_ident(Some(errors::MissingIdentKind::ExpressionExpected))),
        }
    }

    fn parse_class_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let kind = self.parse_class_decl_or_expr(parse_class_like::ParseClassExpr, None)?;
        Ok(self.alloc(ast::Expr {
            kind: ast::ExprKind::Class(kind),
        }))
    }

    fn parse_new_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        let start = self.token.start();
        let id = self.next_node_id();
        self.expect(New)?;
        let expr = self.parse_primary_expr()?;
        let expr = self.parse_member_expr_rest(start as usize, expr)?;
        let ty_args = self.parse_ty_args_in_expr()?;
        if let Some(ty_args) = ty_args {
            if ty_args.list.is_empty() {
                let error = errors::TypeArgumentListCannotBeEmpty { span: ty_args.span };
                self.push_error(Box::new(error));
            }
        }
        let args = if self.token.kind == LParen {
            self.parse_args().map(|args| Some(args))
        } else {
            Ok(None)
        }?;
        let new = self.alloc(ast::NewExpr {
            id,
            span: self.new_span(start),
            expr,
            ty_args,
            args,
        });
        self.insert_map(id, ast::Node::NewExpr(new));
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

    fn parse_object_lit(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        let start = self.token.start();
        self.expect(LBrace)?;
        let id = self.next_node_id();
        let props =
            self.parse_delimited_list(list_ctx::ObjectLitMembers, Self::parse_object_lit_ele);
        self.expect(RBrace)?;
        let lit = self.alloc(ast::ObjectLit {
            id,
            span: self.new_span(start),
            members: props,
        });
        self.insert_map(id, ast::Node::ObjectLit(lit));
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::ObjectLit(lit),
        });
        Ok(expr)
    }

    fn parse_cond_expr_rest(&mut self, cond: &'cx ast::Expr<'cx>) -> PResult<&'cx ast::Expr<'cx>> {
        if self.parse_optional(TokenKind::Question).is_some() {
            let start = cond.span().lo;
            let id = self.next_node_id();
            self.parent_map.r#override(cond.id(), id);
            let when_true = self.with_parent(id, Self::parse_expr)?;
            self.expect(TokenKind::Colon)?;
            let when_false = self.with_parent(id, Self::parse_expr)?;
            let expr = self.alloc(ast::CondExpr {
                id,
                span: self.new_span(start),
                cond,
                when_false,
                when_true,
            });
            self.insert_map(id, ast::Node::CondExpr(expr));
            let expr = self.alloc(ast::Expr {
                kind: ast::ExprKind::Cond(expr),
            });
            Ok(expr)
        } else {
            Ok(cond)
        }
    }

    pub(super) fn parse_left_hand_side_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        self.parse_member_expr()
    }

    fn parse_member_expr(&mut self) -> &'cx ast::Expr<'cx> {
        let start = self.token.start();
        let expr = self.parse_primary_expr().unwrap();
        self.parse_member_expr_rest(start as usize, expr).unwrap()
    }

    fn parse_right_side_dot(&mut self, allow_identifier_names: bool) -> PResult<&'cx ast::Ident> {
        if allow_identifier_names {
            self.parse_ident_name()
        } else {
            todo!()
        }
    }

    fn parse_prop_access_expr_rest(
        &mut self,
        start: usize,
        expr: &'cx ast::Expr<'cx>,
        question_dot_token: bool,
    ) -> PResult<&'cx ast::PropAccessExpr<'cx>> {
        let id = self.next_node_id();
        let name = self.parse_right_side_dot(true)?;
        let prop = if question_dot_token {
            todo!()
        } else {
            let expr = NoParenRule.paren_left_side_of_access(expr, false);
            self.alloc(ast::PropAccessExpr {
                id,
                span: self.new_span(start as u32),
                expr,
                name,
            })
        };
        self.insert_map(id, ast::Node::PropAccessExpr(prop));
        Ok(prop)
    }

    fn parse_ele_access_expr_rest(
        &mut self,
        start: usize,
        expr: &'cx ast::Expr<'cx>,
        question_dot_token: bool,
    ) -> PResult<&'cx ast::EleAccessExpr<'cx>> {
        let id = self.next_node_id();
        if self.token.kind == TokenKind::RBracket {
            return Err(());
        }
        let arg = self.parse_expr()?;
        self.expect(TokenKind::RBracket)?;
        let ele = if question_dot_token {
            todo!()
        } else {
            let expr = NoParenRule.paren_left_side_of_access(expr, false);
            self.alloc(ast::EleAccessExpr {
                id,
                span: self.new_span(start as u32),
                expr,
                arg,
            })
        };
        self.insert_map(id, ast::Node::EleAccessExpr(ele));
        Ok(ele)
    }

    fn parse_member_expr_rest(
        &mut self,
        start: usize,
        mut expr: &'cx ast::Expr<'cx>,
    ) -> PResult<&'cx ast::Expr<'cx>> {
        loop {
            let is_property_access = self.parse_optional(TokenKind::Dot).is_some();
            if is_property_access {
                let prop = self.parse_prop_access_expr_rest(start, expr, false)?;
                expr = self.alloc(ast::Expr {
                    kind: ast::ExprKind::PropAccess(prop),
                });
                continue;
            }

            if self.parse_optional(TokenKind::LBracket).is_some() {
                let ele = self.parse_ele_access_expr_rest(start, expr, false)?;
                expr = self.alloc(ast::Expr {
                    kind: ast::ExprKind::EleAccess(ele),
                });
                continue;
            }

            return Ok(expr);
        }
    }

    fn parse_ty_args_in_expr(&mut self) -> PResult<Option<&'cx ast::Tys<'cx>>> {
        let start = self.token.start();
        if self.re_scan_less() != TokenKind::Less {
            return Ok(None);
        }
        self.next_token();
        let list = self.parse_delimited_list(TypeArguments, Self::parse_ty);
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

    fn can_follow_ty_args_in_expr(&self) -> bool {
        use TokenKind::*;
        if matches!(self.token.kind, LParen) {
            true
        } else if matches!(self.token.kind, Less | Great | Plus | Minus) {
            false
        } else {
            self.has_preceding_line_break() || !self.is_start_of_expr()
        }
    }
}
