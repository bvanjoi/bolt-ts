use super::list_ctx::{self, ListContext};
use super::lookahead::Lookahead;
use super::{PResult, ParserState};
use super::{ast, errors};
use bolt_ts_ast::{Token, TokenKind};

fn is_ele_for_tuple_ele_tys_and_ty_args(s: &mut ParserState) -> bool {
    s.token.kind == TokenKind::Comma || s.is_start_of_ty(false)
}

#[derive(Copy, Clone)]
struct TupleElementTypes;

impl ListContext for TupleElementTypes {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        is_ele_for_tuple_ele_tys_and_ty_args(s)
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        s.token.kind == TokenKind::RBracket
    }
}

#[derive(Copy, Clone)]
pub(super) struct TypeArguments;

impl ListContext for TypeArguments {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        is_ele_for_tuple_ele_tys_and_ty_args(s)
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        s.token.kind != TokenKind::Comma
    }
}

impl<'cx> ParserState<'cx, '_> {
    fn should_parse_ret_ty(&mut self, is_colon: bool, is_ty: bool) -> PResult<bool> {
        if !is_colon {
            self.expect(TokenKind::EqGreat);
            Ok(true)
        } else if self.parse_optional(TokenKind::Colon).is_some() {
            Ok(true)
        } else if is_ty && self.token.kind == TokenKind::EqGreat {
            todo!()
        } else {
            Ok(false)
        }
    }

    pub fn parse_ret_ty(&mut self, is_colon: bool) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self
            .should_parse_ret_ty(is_colon, false)
            .unwrap_or_default()
        {
            self.parse_ty_or_ty_pred().map(Some)
        } else {
            Ok(None)
        }
    }

    pub(super) fn parse_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if self.is_start_of_fn_or_ctor_ty() {
            self.parse_fn_or_ctor_ty()
        } else {
            let start = self.token.start();
            let ty = self.parse_union_ty_or_higher()?;
            if !self.has_preceding_line_break() && self.parse_optional(TokenKind::Extends).is_some()
            {
                let extends_ty = self.disallow_conditional_tys_and(Self::parse_ty)?;
                self.expect(TokenKind::Question);
                let true_ty = self.allow_conditional_tys_and(Self::parse_ty)?;
                self.expect(TokenKind::Colon);
                let false_ty = self.allow_conditional_tys_and(Self::parse_ty)?;
                let id = self.next_node_id();
                let ty = self.alloc(ast::CondTy {
                    id,
                    span: self.new_span(start),
                    check_ty: ty,
                    extends_ty,
                    true_ty,
                    false_ty,
                });
                self.nodes.insert(id, ast::Node::CondTy(ty));
                let ty = self.alloc(ast::Ty {
                    kind: ast::TyKind::Cond(ty),
                });
                Ok(ty)
            } else {
                Ok(ty)
            }
        }
    }

    fn parse_union_or_intersection_ty<const IS_UNION_TY: bool>(
        &mut self,
        expect: TokenKind,
        parse_constituent_type: impl FnOnce(&mut Self) -> PResult<&'cx ast::Ty<'cx>> + Copy,
    ) -> PResult<&'cx ast::Ty<'cx>> {
        let has_leading_operator = self.parse_optional(expect).is_some();
        let ty = if has_leading_operator {
            if let Some(ty) = self.parse_fn_or_ctor_ty_to_error(IS_UNION_TY)? {
                ty
            } else {
                parse_constituent_type(self)?
            }
        } else {
            parse_constituent_type(self)?
        };

        if self.token.kind == expect {
            let mut tys = vec![ty];
            // self.parent_map.r#override(ty.id(), parent);
            while self.parse_optional(expect).is_some() {
                if let Some(ty) = self.parse_fn_or_ctor_ty_to_error(IS_UNION_TY)? {
                    tys.push(ty);
                } else {
                    let ty = parse_constituent_type(self)?;
                    tys.push(ty);
                }
            }

            let tys = self.alloc(tys);
            let ty = if expect == TokenKind::Amp {
                let parent = self.next_node_id();
                let ty = self.alloc(ast::IntersectionTy {
                    id: parent,
                    span: self.new_span(ty.span().lo),
                    tys,
                });
                self.nodes.insert(parent, ast::Node::IntersectionTy(ty));
                self.alloc(ast::Ty {
                    kind: ast::TyKind::Intersection(ty),
                })
            } else {
                // union
                assert_eq!(expect, TokenKind::Pipe);
                let parent = self.next_node_id();
                let ty = self.alloc(ast::UnionTy {
                    id: parent,
                    span: self.new_span(ty.span().lo),
                    tys,
                });
                self.nodes.insert(parent, ast::Node::UnionTy(ty));
                self.alloc(ast::Ty {
                    kind: ast::TyKind::Union(ty),
                })
            };
            Ok(ty)
        } else {
            Ok(ty)
        }
    }

    fn parse_intersection_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_union_or_intersection_ty::<false>(TokenKind::Amp, Self::parse_ty_op_or_higher)
    }

    fn parse_union_ty_or_higher(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_union_or_intersection_ty::<true>(TokenKind::Pipe, Self::parse_intersection_ty)
    }

    fn parse_ty_pred_prefix(&mut self) -> PResult<Option<&'cx ast::Ident>> {
        let id = self.create_ident(true, None);
        if self.token.kind == TokenKind::Is && !self.has_preceding_line_break() {
            self.next_token();
            Ok(Some(id))
        } else {
            Err(())
        }
    }

    fn parse_this_ty_pred(&mut self, this_ty: &'cx ast::ThisTy) -> PResult<&'cx ast::Ty<'cx>> {
        let start = this_ty.span.lo;
        self.next_token();
        let ty = self.parse_ty()?;
        let name = ast::PredTyName::This(this_ty);
        let id = self.next_node_id();
        let ty = self.alloc(ast::PredTy {
            id,
            span: self.new_span(start),
            asserts: None,
            name,
            ty: Some(ty),
        });
        self.nodes.insert(id, ast::Node::PredTy(ty));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Pred(ty),
        });
        Ok(ty)
    }

    pub(super) fn parse_ty_or_ty_pred(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if self.token.kind.is_ident() {
            let start = self.token.start();
            if let Ok(Some(name)) = self.try_parse(|l| l.p.parse_ty_pred_prefix()) {
                let ty = self.parse_ty()?;
                let id = self.next_node_id();
                let name = ast::PredTyName::Ident(name);
                let ty = self.alloc(ast::PredTy {
                    id,
                    span: self.new_span(start),
                    asserts: None,
                    name,
                    ty: Some(ty),
                });
                self.nodes.insert(id, ast::Node::PredTy(ty));
                let ty = self.alloc(ast::Ty {
                    kind: ast::TyKind::Pred(ty),
                });
                return Ok(ty);
            }
        }
        self.parse_ty()
    }

    fn parse_fn_or_ctor_ty_to_error(
        &mut self,
        is_union_ty: bool,
    ) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.is_start_of_fn_or_ctor_ty() {
            let ty = self.parse_fn_or_ctor_ty()?;
            // TODO: error
            Ok(Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_modifiers_for_ctor_ty(&mut self) -> PResult<Option<&'cx ast::Modifiers<'cx>>> {
        if self.token.kind == TokenKind::Abstract {
            let pos = self.token.start();
            let m = self.parse_modifier(false, None)?.unwrap();
            let m = self.alloc(ast::Modifiers {
                span: self.new_span(pos),
                flags: ast::ModifierKind::Abstract.into(),
                list: self.alloc(vec![m]),
            });
            Ok(Some(m))
        } else {
            Ok(None)
        }
    }

    fn parse_fn_or_ctor_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let modifiers = self.parse_modifiers_for_ctor_ty()?;
        let is_ctor_ty = self.parse_optional(TokenKind::New).is_some();
        assert!(modifiers.is_none() || is_ctor_ty);
        let ty_params = self.parse_ty_params()?;
        let params = self.parse_params()?;
        self.check_params(params, false);
        let ty = self.parse_ret_ty(false)?.unwrap();
        let ty = if is_ctor_ty {
            let id = self.next_node_id();
            let ctor_ty = self.alloc(ast::CtorTy {
                id,
                span: self.new_span(start),
                modifiers,
                ty_params,
                params,
                ty,
            });
            self.nodes.insert(id, ast::Node::CtorTy(ctor_ty));
            self.alloc(ast::Ty {
                kind: ast::TyKind::Ctor(ctor_ty),
            })
        } else {
            let id = self.next_node_id();
            let fn_ty = self.alloc(ast::FnTy {
                id,
                span: self.new_span(start),
                ty_params,
                params,
                ty,
            });
            self.nodes.insert(id, ast::Node::FnTy(fn_ty));
            self.alloc(ast::Ty {
                kind: ast::TyKind::Fn(fn_ty),
            })
        };

        Ok(ty)
    }

    pub(super) fn parse_ty_anno(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon).is_some() {
            self.parse_ty().map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse_infer_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Infer);
        let ty_param = self.parse_ty_param_of_infer_ty()?;
        let id = self.next_node_id();
        let ty = self.alloc(ast::InferTy {
            id,
            span: self.new_span(start),
            ty_param,
        });
        self.nodes.insert(id, ast::Node::InferTy(ty));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Infer(ty),
        });
        Ok(ty)
    }

    fn parse_ty_param_of_infer_ty(&mut self) -> PResult<&'cx ast::TyParam<'cx>> {
        let start = self.token.start();
        let name = self.create_ident(true, None);
        let constraint = self.try_parse(|l| l.p.try_parse_constraint_of_infer_ty())?;
        let id = self.next_node_id();
        let ty = self.alloc(ast::TyParam {
            id,
            span: self.new_span(start),
            name,
            constraint,
            default: None,
        });
        self.nodes.insert(id, ast::Node::TyParam(ty));
        Ok(ty)
    }

    fn try_parse_constraint_of_infer_ty(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Extends).is_some() {
            let constraint = self.disallow_conditional_tys_and(Self::parse_ty)?;
            if self.in_disallow_conditional_tys_context() || self.token.kind != TokenKind::Question
            {
                Ok(Some(constraint))
            } else {
                Err(())
            }
        } else {
            Ok(None)
        }
    }

    fn parse_ty_op_or_higher(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        match self.token.kind {
            TokenKind::Keyof | TokenKind::Readonly | TokenKind::Unique => {
                let op = self.token.kind.try_into().unwrap();
                self.parse_ty_op(op)
            }
            TokenKind::Infer => self.parse_infer_ty(),
            _ => self.allow_conditional_tys_and(Self::parse_postfix_ty),
        }
    }

    fn parse_ty_op(&mut self, op: ast::TyOpKind) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        self.next_token();
        let ty = self.parse_ty_op_or_higher()?;
        let id = self.next_node_id();
        let ty = self.alloc(ast::TyOp {
            id,
            span: self.new_span(start),
            op,
            ty,
        });
        self.nodes.insert(id, ast::Node::TyOp(ty));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::TyOp(ty),
        });
        Ok(ty)
    }

    fn parse_postfix_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let mut ty = self.parse_non_array_ty()?;
        loop {
            if self.has_preceding_line_break() {
                return Ok(ty);
            }
            match self.token.kind {
                TokenKind::Excl => {
                    todo!()
                }
                TokenKind::Question => {
                    if (Lookahead { p: self }).lookahead(Lookahead::next_token_is_start_of_expr) {
                        return Ok(ty);
                    } else {
                        self.next_token();
                        let id = self.next_node_id();
                        let n = self.alloc(ast::NullableTy {
                            id,
                            span: self.new_span(start),
                            ty,
                        });
                        self.nodes.insert(id, ast::Node::NullableTy(n));
                        ty = self.alloc(ast::Ty {
                            kind: ast::TyKind::Nullable(n),
                        })
                    }
                }
                TokenKind::LBracket => {
                    self.expect(TokenKind::LBracket);
                    if self.is_start_of_ty(false) {
                        let index_ty = self.parse_ty()?;
                        self.expect(TokenKind::RBracket);
                        let id = self.next_node_id();
                        let kind = self.alloc(ast::IndexedAccessTy {
                            id,
                            span: self.new_span(start),
                            ty,
                            index_ty,
                        });
                        self.nodes.insert(id, ast::Node::IndexedAccessTy(kind));
                        ty = self.alloc(ast::Ty {
                            kind: ast::TyKind::IndexedAccess(kind),
                        });
                    } else {
                        self.expect(TokenKind::RBracket);
                        let id = self.next_node_id();
                        let kind = self.alloc(ast::ArrayTy {
                            id,
                            span: self.new_span(ty.span().lo),
                            ele: ty,
                        });
                        self.nodes.insert(id, ast::Node::ArrayTy(kind));
                        ty = self.alloc(ast::Ty {
                            kind: ast::TyKind::Array(kind),
                        });
                    }
                }
                _ => return Ok(ty),
            };
        }
    }

    pub(super) fn parse_right_side_of_dot(
        &mut self,
        allow_identifier_name: bool,
    ) -> PResult<&'cx ast::Ident> {
        if self.has_preceding_line_break() && self.token.kind.is_ident_or_keyword() {
            let matches_pattern =
                self.lookahead(Lookahead::next_token_is_ident_or_keyword_on_same_line)?;
            if matches_pattern {
                todo!()
            }
        }
        if allow_identifier_name {
            self.parse_ident_name()
        } else {
            Ok(self.create_ident(self.is_ident(), None))
        }
    }

    pub(super) fn parse_entity_name(
        &mut self,
        allow_reserved_word: bool,
    ) -> PResult<&'cx ast::EntityName<'cx>> {
        let start = self.token.start();
        let name = self.parse_ident_name()?;
        let kind = ast::EntityNameKind::Ident(name);
        let mut entity = self.alloc(ast::EntityName { kind });
        if self.token.kind != TokenKind::Dot {
            return Ok(entity);
        }
        // self.parent_map.r#override(name.id, id);
        while self.parse_optional(TokenKind::Dot).is_some() {
            if self.token.kind == TokenKind::Less {
                break;
            }
            let right = self.parse_right_side_of_dot(allow_reserved_word)?;
            let id = self.next_node_id();
            let qualified = self.alloc(ast::QualifiedName {
                id,
                span: self.new_span(start),
                left: entity,
                right,
            });
            self.nodes.insert(id, ast::Node::QualifiedName(qualified));
            entity = self.alloc(ast::EntityName {
                kind: ast::EntityNameKind::Qualified(qualified),
            });
        }
        Ok(entity)
    }

    fn parse_ty_args_of_ty_reference(&mut self) -> PResult<Option<&'cx ast::Tys<'cx>>> {
        if !self.has_preceding_line_break() && self.re_scan_less() == TokenKind::Less {
            let start = self.token.start();
            let list = self
                .parse_bracketed_list(
                    TypeArguments,
                    TokenKind::Less,
                    Self::parse_ty,
                    TokenKind::Great,
                )
                .unwrap();
            let tys = self.alloc(ast::Tys {
                span: self.new_span(start),
                list,
            });
            Ok(Some(tys))
        } else {
            Ok(None)
        }
    }

    pub(super) fn parse_entity_name_of_ty_reference(&mut self) -> PResult<&'cx ast::ReferTy<'cx>> {
        let start = self.token.start();
        let name = self.parse_entity_name(true)?;
        let ty_args = self.parse_ty_args_of_ty_reference()?;
        let id = self.next_node_id();
        let ty = self.alloc(ast::ReferTy {
            id,
            span: self.new_span(start),
            name,
            ty_args,
        });
        self.nodes.insert(id, ast::Node::ReferTy(ty));
        Ok(ty)
    }

    fn parse_ty_reference(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let refer = self.parse_entity_name_of_ty_reference()?;
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Refer(refer),
        });
        Ok(ty)
    }

    fn parse_keyword_and_not_dot(&mut self) -> PResult<Option<Token>> {
        let token = self.parse_token_node();
        Ok((self.token.kind != TokenKind::Dot).then_some(token))
    }

    pub(super) fn try_parse_ty_args(&mut self) -> PResult<Option<&'cx ast::Tys<'cx>>> {
        if self.token.kind == TokenKind::Less {
            let start = self.token.start();
            let tys = self.parse_bracketed_list(
                list_ctx::TyArgs,
                TokenKind::Less,
                Self::parse_ty,
                TokenKind::Great,
            )?;
            let tys = self.alloc(ast::Tys {
                span: self.new_span(start),
                list: tys,
            });
            Ok(Some(tys))
        } else {
            Ok(None)
        }
    }

    fn parse_ty_query(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Typeof);
        let name = self.parse_entity_name(true)?;
        let ty_args = if self.has_preceding_line_break() {
            self.try_parse_ty_args()?
        } else {
            None
        };
        let id = self.next_node_id();
        let kind = self.alloc(ast::TypeofTy {
            id,
            span: self.new_span(start),
            name,
            ty_args,
        });
        self.nodes.insert(id, ast::Node::TypeofTy(kind));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Typeof(kind),
        });
        Ok(ty)
    }

    fn parse_this_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::This);
        let id = self.next_node_id();
        let kind = self.alloc(ast::ThisTy {
            id,
            span: self.new_span(start),
        });
        self.nodes.insert(id, ast::Node::ThisTy(kind));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::This(kind),
        });
        Ok(ty)
    }

    fn parse_non_array_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        let t = self.token.kind;
        match t {
            True | False | Null | Void | Undefined => {
                let kind = match self.token.kind {
                    True => ast::LitTyKind::True,
                    False => ast::LitTyKind::False,
                    Null => ast::LitTyKind::Null,
                    Void => ast::LitTyKind::Void,
                    Undefined => ast::LitTyKind::Undefined,
                    _ => unreachable!(),
                };
                let lit = self.create_lit_ty(kind, self.token.span);
                self.nodes.insert(lit.id, ast::Node::LitTy(lit));
                self.next_token();
                let ty = self.alloc(ast::Ty {
                    kind: ast::TyKind::Lit(lit),
                });
                Ok(ty)
            }
            Number | String | BigInt | NoSubstitutionTemplate => self.parse_literal_ty(false),
            Typeof => {
                if (Lookahead { p: self }).lookahead(Lookahead::is_start_of_ty_of_import_ty) {
                    todo!()
                } else {
                    self.parse_ty_query()
                }
            }
            LParen => self.parse_paren_ty(),
            LBrace => {
                if (Lookahead { p: self })
                    .lookahead(Lookahead::is_start_of_mapped_ty)
                    .unwrap_or_default()
                {
                    self.parse_mapped_ty()
                } else {
                    self.parse_ty_lit()
                }
            }
            LBracket => self.parse_tuple_ty(),
            Minus => {
                if (Lookahead { p: self })
                    .lookahead(Lookahead::next_token_is_numeric_or_big_int_literal)
                {
                    self.parse_literal_ty(true)
                } else {
                    todo!()
                }
            }
            TemplateHead => self.parse_template_ty(),
            This => {
                let this_ty = self.parse_this_ty()?;
                if self.token.kind == TokenKind::Is && !self.has_preceding_line_break() {
                    if let ast::TyKind::This(this_ty) = this_ty.kind {
                        self.parse_this_ty_pred(this_ty)
                    } else {
                        unreachable!()
                    }
                } else {
                    Ok(this_ty)
                }
            }
            Asserts
                if self
                    .lookahead(Lookahead::next_token_is_ident_or_keyword_on_same_line)
                    .unwrap_or_default() =>
            {
                self.parse_asserts_ty_pred()
            }
            _ => self.parse_ty_reference(),
        }
    }

    fn parse_asserts_ty_pred(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let pos = self.token.start();
        let assert_modifier = self.token.span;
        self.expect(TokenKind::Asserts);
        let name = if self.token.kind == TokenKind::This {
            if let ast::TyKind::This(this_ty) = self.parse_this_ty()?.kind {
                ast::PredTyName::This(this_ty)
            } else {
                unreachable!()
            }
        } else {
            let is_ident = self.token.kind == TokenKind::Ident;
            ast::PredTyName::Ident(self.create_ident(is_ident, None))
        };
        let ty = if self.parse_optional(TokenKind::Is).is_some() {
            Some(self.parse_ty()?)
        } else {
            None
        };
        let id = self.next_node_id();
        let ty = self.alloc(ast::PredTy {
            id,
            span: self.new_span(pos),
            name,
            ty,
            asserts: Some(assert_modifier),
        });
        self.nodes.insert(id, ast::Node::PredTy(ty));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Pred(ty),
        });
        Ok(ty)
    }

    fn parse_literal_ty(&mut self, neg: bool) -> PResult<&'cx ast::Ty<'cx>> {
        if neg {
            self.expect(TokenKind::Minus);
        }
        use bolt_ts_ast::TokenKind::*;
        let token_val = self.token_value.unwrap();
        if let Some(node) = self.try_parse(|l| l.p.parse_keyword_and_not_dot())? {
            let ty = match node.kind {
                Number => {
                    let val = token_val.number();
                    let val = if neg { -val } else { val };
                    let kind = ast::LitTyKind::Num(val);
                    let lit = self.create_lit_ty(kind, self.token.span);
                    self.nodes.insert(lit.id, ast::Node::LitTy(lit));
                    self.alloc(ast::Ty {
                        kind: ast::TyKind::Lit(lit),
                    })
                }
                BigInt => {
                    let val = token_val.ident();
                    let kind = ast::LitTyKind::BigInt { val, neg };
                    let lit = self.create_lit_ty(kind, self.token.span);
                    self.nodes.insert(lit.id, ast::Node::LitTy(lit));
                    self.alloc(ast::Ty {
                        kind: ast::TyKind::Lit(lit),
                    })
                }
                String | NoSubstitutionTemplate => {
                    let val = token_val.ident();
                    let kind = ast::LitTyKind::String(val);
                    let lit = self.create_lit_ty(kind, self.token.span);
                    self.nodes.insert(lit.id, ast::Node::LitTy(lit));
                    self.alloc(ast::Ty {
                        kind: ast::TyKind::Lit(lit),
                    })
                }

                _ => unreachable!(),
            };
            Ok(ty)
        } else {
            self.parse_ty_reference()
        }
    }

    fn parse_template_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let head = self.parse_template_head(false)?;
        let spans = self
            .parse_template_spans(|this| this.parse_template_ty_span().map(|n| (n, !n.is_tail)))?;
        let id = self.next_node_id();
        let kind = self.alloc(ast::TemplateLitTy {
            id,
            span: self.new_span(start),
            head,
            spans,
        });
        self.nodes.insert(id, ast::Node::TemplateLitTy(kind));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::TemplateLit(kind),
        });
        Ok(ty)
    }

    fn parse_template_ty_span(&mut self) -> PResult<&'cx ast::TemplateSpanTy<'cx>> {
        let start = self.token.start();
        let ty = self.parse_ty()?;
        let (text, is_tail) = self.parse_template_span_text(false);
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateSpanTy {
            id,
            span: self.new_span(start),
            ty,
            text,
            is_tail,
        });
        self.nodes.insert(node.id, ast::Node::TemplateSpanTy(node));
        Ok(node)
    }

    fn parse_mapped_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBrace);
        let mut readonly_token = None;
        if matches!(
            self.token.kind,
            TokenKind::Readonly | TokenKind::Plus | TokenKind::Minus
        ) {
            let t = self.parse_token_node();
            readonly_token = Some(t);
            if matches!(t.kind, TokenKind::Plus | TokenKind::Minus) {
                readonly_token = Some(self.token);
                self.expect(TokenKind::Readonly);
            }
        }
        self.expect(TokenKind::LBracket);
        let ty_param = self.parse_mapped_ty_param()?;
        let name_ty = if self.parse_optional(TokenKind::As).is_some() {
            let ty = self.parse_ty()?;
            Some(ty)
        } else {
            None
        };
        self.expect(TokenKind::RBracket);
        let mut question_token = None;
        if matches!(
            self.token.kind,
            TokenKind::Question | TokenKind::Plus | TokenKind::Minus
        ) {
            let t = self.parse_token_node();
            question_token = Some(t);
            if t.kind != TokenKind::Question {
                self.expect(TokenKind::Question);
            };
        }

        let ty = self.parse_ty_anno()?;
        self.parse_semi();
        let members = self.parse_list(list_ctx::TyMembers, Self::parse_ty_member);
        self.expect(TokenKind::RBrace);
        let id = self.next_node_id();
        let kind = self.alloc(ast::MappedTy {
            id,
            span: self.new_span(start),
            readonly_token,
            ty_param,
            name_ty,
            question_token,
            ty,
            members,
        });
        self.nodes.insert(id, ast::Node::MappedTy(kind));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Mapped(kind),
        });
        Ok(ty)
    }

    fn parse_mapped_ty_param(&mut self) -> PResult<&'cx ast::TyParam<'cx>> {
        let start = self.token.start();
        let name = self.create_ident(true, None);
        self.expect(TokenKind::In);
        let constraint = self.parse_ty()?;
        let id = self.next_node_id();
        let ty = self.alloc(ast::TyParam {
            id,
            span: self.new_span(start),
            name,
            constraint: Some(constraint),
            default: None,
        });
        self.nodes.insert(id, ast::Node::TyParam(ty));
        Ok(ty)
    }

    fn parse_tuple_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let tys = self.parse_bracketed_list(
            TupleElementTypes,
            TokenKind::LBracket,
            Self::parse_tuple_ele_name_or_tuple_ele_ty,
            TokenKind::RBracket,
        )?;
        let id = self.next_node_id();
        let ty = self.alloc(ast::TupleTy {
            id,
            span: self.new_span(start),
            tys,
        });
        self.nodes.insert(id, ast::Node::TupleTy(ty));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Tuple(ty),
        });
        Ok(ty)
    }

    fn parse_tuple_ele_name_or_tuple_ele_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if (Lookahead { p: self }).lookahead(Lookahead::is_tuple_ele_name) {
            let start = self.token.start();
            let dotdotdot = self.parse_optional(TokenKind::DotDotDot);
            let name = self.parse_ident_name()?;
            let question = self.parse_optional(TokenKind::Question);
            self.expect(TokenKind::Colon);
            let ty = self.parse_tuple_ele_ty()?;
            let id = self.next_node_id();
            let ty = self.alloc(ast::NamedTupleTy {
                id,
                span: self.new_span(start),
                dotdotdot: dotdotdot.map(|t| t.span),
                name,
                question: question.map(|t| t.span),
                ty,
            });
            self.nodes.insert(id, ast::Node::NamedTupleTy(ty));
            let ty = self.alloc(ast::Ty {
                kind: ast::TyKind::NamedTuple(ty),
            });
            Ok(ty)
        } else {
            self.parse_tuple_ele_ty()
        }
    }

    fn parse_tuple_ele_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if self.parse_optional(TokenKind::DotDotDot).is_some() {
            let pos = self.token.start();
            let ty = self.parse_ty()?;
            let id = self.next_node_id();
            let ty = self.alloc(ast::RestTy {
                id,
                span: self.new_span(pos),
                ty,
            });
            self.nodes.insert(id, ast::Node::RestTy(ty));
            let ty = self.alloc(ast::Ty {
                kind: ast::TyKind::Rest(ty),
            });
            Ok(ty)
        } else {
            self.parse_ty()
        }
    }

    fn parse_ty_lit(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let members = self.parse_object_ty_members()?;
        let id = self.next_node_id();
        let kind = self.alloc(ast::ObjectLitTy {
            id,
            span: self.new_span(start),
            members,
        });
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::ObjectLit(kind),
        });
        self.nodes.insert(id, ast::Node::ObjectLitTy(kind));
        Ok(ty)
    }

    fn parse_paren_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LParen);
        let ty = self.parse_ty()?;
        self.expect(TokenKind::RParen);
        let id = self.next_node_id();
        let kind = self.alloc(ast::ParenTy {
            id,
            span: self.new_span(start),
            ty,
        });
        self.nodes.insert(id, ast::Node::ParenTy(kind));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Paren(kind),
        });
        Ok(ty)
    }

    fn parse_prop_or_method_sig(
        &mut self,
        start: u32,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        let name = self.parse_prop_name(true)?;
        let question = self.parse_optional(TokenKind::Question).map(|t| t.span);
        let kind = if matches!(self.token.kind, TokenKind::LParen | TokenKind::Less) {
            let ty_params = self.parse_ty_params()?;
            let params = self.parse_params()?;
            self.check_params(params, false);
            let ty = self.parse_ret_ty(true)?;
            let id = self.next_node_id();
            let sig = self.alloc(ast::MethodSignature {
                id,
                span: self.new_span(start),
                name,
                question,
                ty_params,
                params,
                ty,
            });
            self.nodes.insert(id, ast::Node::MethodSignature(sig));
            ast::ObjectTyMemberKind::Method(sig)
        } else {
            let ty = self.parse_ty_anno()?;
            let id = self.next_node_id();
            let sig = self.alloc(ast::PropSignature {
                id,
                span: self.new_span(start),
                modifiers,
                name,
                question,
                ty,
            });
            self.nodes.insert(id, ast::Node::PropSignature(sig));
            if let Some(init) = self.parse_init()? {
                let error =
                    errors::AnInterfacePropertyCannotHaveAnInitializer { span: init.span() };
                self.push_error(Box::new(error));
            }
            ast::ObjectTyMemberKind::Prop(sig)
        };
        let node = self.alloc(ast::ObjectTyMember { kind });
        self.parse_ty_member_semi();
        Ok(node)
    }

    fn parse_sig_member(&mut self, is_call: bool) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        let start = self.token.start();

        if !is_call {
            self.expect(TokenKind::New);
        }

        let ty_params = self.parse_ty_params()?;
        let params = self.parse_params()?;
        self.check_params(params, false);
        let ty = self.parse_ret_ty(true)?;
        self.parse_ty_member_semi();
        let span = self.new_span(start);
        let kind = if is_call {
            let id = self.next_node_id();
            let decl = self.alloc(ast::CallSigDecl {
                id,
                span,
                ty_params,
                params,
                ty,
            });
            self.nodes.insert(id, ast::Node::CallSigDecl(decl));
            ast::ObjectTyMemberKind::CallSig(decl)
        } else {
            let id = self.next_node_id();
            let decl = self.alloc(ast::CtorSigDecl {
                id,
                span,
                ty_params,
                params,
                ty,
            });
            self.nodes.insert(id, ast::Node::CtorSigDecl(decl));
            ast::ObjectTyMemberKind::CtorSig(decl)
        };
        Ok(self.alloc(ast::ObjectTyMember { kind }))
    }

    fn parse_ty_member(&mut self) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        if self.token.kind == TokenKind::LParen || self.token.kind == TokenKind::Less {
            return self.parse_sig_member(true);
        } else if self.token.kind == TokenKind::New {
            return self.parse_sig_member(false);
        }

        let start = self.token.start() as usize;
        let modifiers = self.parse_modifiers(false, None)?;

        if self.parse_contextual_modifier(TokenKind::Get) {
            let decl = self.parse_getter_accessor_decl(start, modifiers, true)?;
            Ok(self.alloc(ast::ObjectTyMember {
                kind: ast::ObjectTyMemberKind::Getter(decl),
            }))
        } else if self.parse_contextual_modifier(TokenKind::Set) {
            let decl = self.parse_setter_accessor_decl(start, modifiers, true)?;
            Ok(self.alloc(ast::ObjectTyMember {
                kind: ast::ObjectTyMemberKind::Setter(decl),
            }))
        } else if self.is_index_sig() {
            let decl = self.parse_index_sig_decl(start, modifiers)?;
            Ok(self.alloc(ast::ObjectTyMember {
                kind: ast::ObjectTyMemberKind::IndexSig(decl),
            }))
        } else {
            self.parse_prop_or_method_sig(start as u32, modifiers)
        }
    }

    pub(super) fn parse_object_ty_members(&mut self) -> PResult<ast::ObjectTyMembers<'cx>> {
        self.expect(TokenKind::LBrace);
        let members = self.parse_list(list_ctx::TyMembers, Self::parse_ty_member);
        self.expect(TokenKind::RBrace);
        Ok(members)
    }
}
