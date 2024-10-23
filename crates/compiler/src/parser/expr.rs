use crate::ast::ExprKind;

use super::ast::{self, BinOp};
use super::list_ctx::{self, ListContext};
use super::token::{BinPrec, TokenKind};
use super::{PResult, ParserState};

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    fn is_update_expr(&self) -> bool {
        true
    }

    pub(super) fn parse_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        self.parse_assign_expr()
    }

    pub(super) fn parse_init(&mut self) -> Option<&'cx ast::Expr<'cx>> {
        self.parse_optional(TokenKind::Eq)
            .map(|_| self.parse_assign_expr().unwrap())
    }

    fn parse_assign_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let start = self.token.start();
        let expr = self.parse_binary_expr(BinPrec::Lowest);
        if let ExprKind::Ident(binding) = expr.kind {
            if self.token.kind.is_assignment() {
                let id = self.p.next_node_id();
                let kind = self.with_parent(id, |this| {
                    let id = this.p.next_node_id();
                    this.p.parent_map.r#override(expr.id, id);
                    let op = this.token.kind.into_assign_op();
                    this.parse_token_node();
                    let right = this.parse_assign_expr()?;
                    let expr = this.alloc(ast::AssignExpr {
                        id,
                        binding,
                        op,
                        right,
                        span: this.new_span(start as usize, this.pos),
                    });
                    this.insert_map(id, ast::Node::AssignExpr(expr));
                    Ok(expr)
                })?;
                let expr = self.alloc(ast::Expr {
                    id,
                    kind: ast::ExprKind::Assign(&kind),
                });
                self.insert_map(id, ast::Node::Expr(expr));
                return Ok(expr);
            }
        }

        self.parse_cond_expr_rest(expr)
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
                this.insert_map(bin_expr_id, ast::Node::BinExpr(bin_expr));
            });
            let kind = ast::ExprKind::Bin(bin_expr);
            left = self.alloc(ast::Expr { id: expr_id, kind });
            self.insert_map(expr_id, ast::Node::Expr(left));
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
                    this.insert_map(id, ast::Node::CallExpr(call));
                    Ok(call)
                })?;
                expr = self.alloc(ast::Expr {
                    id,
                    kind: ast::ExprKind::Call(kind),
                });
                self.insert_map(id, ast::Node::Expr(expr));
            } else {
                break Ok(expr);
            }
        }
    }

    fn parse_args(&mut self) -> PResult<ast::Exprs<'cx>> {
        self.expect(TokenKind::LParen)?;
        let args = self.parse_delimited_list(
            list_ctx::ArgExprs::is_ele,
            Self::parse_arg,
            list_ctx::ArgExprs::is_closing,
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
        self.insert_map(id, ast::Node::ObjectMemberField(filed));
        Ok(filed)
    }

    fn parse_prop_name(&mut self) -> PResult<&'cx ast::PropName<'cx>> {
        let id = self.p.next_node_id();
        let ident = self.with_parent(id, Self::parse_ident_name)?;
        let prop_name = self.alloc(ast::PropName {
            id,
            kind: ast::PropNameKind::Ident(ident),
        });
        self.insert_map(id, ast::Node::PropName(prop_name));
        Ok(prop_name)
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
            this.insert_map(id, ast::Node::ParenExpr(expr));
            Ok(expr)
        })?;
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::Paren(kind),
        });
        Ok(expr)
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
        self.with_parent(expr_id, |this| {
            this.insert_map(id, ast::Node::ArrayLit(&lit))
        });
        let expr = self.alloc(ast::Expr {
            id: expr_id,
            kind: ast::ExprKind::ArrayLit(lit),
        });
        expr
    }

    fn parse_array_lit_elems(&mut self) -> &'cx [&'cx ast::Expr<'cx>] {
        self.parse_delimited_list(
            |s| matches!(s.token.kind, TokenKind::Comma) || s.token.kind.is_start_of_expr(),
            |this| {
                if this.token.kind == TokenKind::Comma {
                    let id = this.p.next_node_id();
                    let kind = this.with_parent(id, |this| {
                        let id = this.p.next_node_id();
                        let expr = this.alloc(ast::OmitExpr {
                            id,
                            span: this.token.span,
                        });
                        this.insert_map(id, ast::Node::OmitExpr(expr));
                        expr
                    });
                    let expr = this.alloc(ast::Expr {
                        id,
                        kind: ast::ExprKind::Omit(&kind),
                    });
                    this.insert_map(id, ast::Node::Expr(expr));
                    Ok(expr)
                } else {
                    this.parse_assign_expr()
                }
            },
            |s| s.token.kind == TokenKind::RBracket,
        )
    }

    fn parse_lit(&mut self) -> &'cx ast::Expr<'cx> {
        let id = self.p.next_node_id();
        let expr = self.with_parent(id, |this| {
            use TokenKind::*;
            let kind = match this.token.kind {
                Number => {
                    let num = this.number_token();
                    let lit = this.create_lit(num, this.token.span);
                    this.insert_map(lit.id, ast::Node::NumLit(lit));
                    ast::ExprKind::NumLit(lit)
                }
                False | True => {
                    let v = this.token.kind == True;
                    let lit = this.create_lit(v, this.token.span);
                    this.insert_map(lit.id, ast::Node::BoolLit(lit));
                    ast::ExprKind::BoolLit(lit)
                }
                Null => {
                    let lit = this.create_lit((), this.token.span);
                    this.insert_map(lit.id, ast::Node::NullLit(lit));
                    ast::ExprKind::NullLit(lit)
                }
                String | NoSubstitutionTemplate => {
                    let s = this.string_token();
                    let lit = this.create_lit(s, this.token.span);
                    this.insert_map(lit.id, ast::Node::StringLit(lit));
                    ast::ExprKind::StringLit(lit)
                }
                _ => unreachable!(),
            };
            let expr = this.alloc(ast::Expr { id, kind });
            expr
        });
        self.insert_map(id, ast::Node::Expr(expr));
        expr
    }

    fn parse_primary_expr(&mut self) -> &'cx ast::Expr<'cx> {
        use TokenKind::*;
        match self.token.kind {
            NoSubstitutionTemplate | String | Number | True | False | Null => self.parse_lit(),
            LBracket => self.parse_array_lit(),
            LParen => self.parse_paren_expr().unwrap(),
            LBrace => self.parse_object_lit().unwrap(),
            Function => self.parse_fn_expr().unwrap(),
            New => self.parse_new_expr().unwrap(),
            _ => self.parse_ident(),
        }
    }

    fn parse_new_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(New)?;
        let kind = self.with_parent(id, |this| {
            let id = this.p.next_node_id();
            let expr = this.parse_primary_expr();
            let expr = this.parse_member_expr_rest(start as usize, expr)?;
            let args = if this.token.kind == TokenKind::LParen {
                this.parse_args().map(|args| Some(args))
            } else {
                Ok(None)
            }?;
            let new = this.alloc(ast::NewExpr {
                id,
                span: this.new_span(start as usize, this.pos),
                expr,
                args,
            });
            this.insert_map(id, ast::Node::NewExpr(new));
            Ok(new)
        })?;
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::New(kind),
        });
        self.insert_map(id, ast::Node::Expr(expr));
        Ok(expr)
    }

    fn parse_fn_expr(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(Function)?;
        let kind = self.with_parent(id, |this| {
            let id = this.p.next_node_id();
            let name = this.parse_optional_binding_ident()?;
            let params = this.parse_params()?;
            let ret_ty = this.parse_ret_ty(true)?;
            let body = this.parse_fn_block()?;
            let f = this.alloc(ast::FnExpr {
                id,
                span: this.new_span(start as usize, this.pos),
                name,
                params,
                ret_ty,
                body,
            });
            this.insert_map(id, ast::Node::FnExpr(f));
            Ok(f)
        })?;
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::Fn(kind),
        });
        self.insert_map(id, ast::Node::Expr(expr));
        Ok(expr)
    }

    fn parse_object_lit(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        use TokenKind::*;
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(LBrace)?;
        let kind = self.with_parent(id, |this| {
            let id = this.p.next_node_id();
            let props = this.parse_delimited_list(
                list_ctx::ObjectLitMembers::is_ele,
                Self::parse_object_lit_ele,
                list_ctx::ObjectLitMembers::is_closing,
            );
            this.expect(RBrace)?;
            let lit = this.alloc(ast::ObjectLit {
                id,
                span: this.new_span(start as usize, this.pos),
                members: props,
            });
            this.insert_map(id, ast::Node::ObjectLit(lit));
            Ok(lit)
        })?;
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::ObjectLit(kind),
        });
        self.insert_map(id, ast::Node::Expr(expr));
        Ok(expr)
    }

    fn parse_cond_expr_rest(&mut self, cond: &'cx ast::Expr<'cx>) -> PResult<&'cx ast::Expr<'cx>> {
        if self.parse_optional(TokenKind::Question).is_some() {
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
                this.insert_map(id, ast::Node::CondExpr(expr));
                Ok(expr)
            })?;
            let expr = self.alloc(ast::Expr {
                id,
                kind: ast::ExprKind::Cond(kind),
            });
            self.insert_map(id, ast::Node::Expr(expr));
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
}
