use super::ast;
use super::list_ctx::{self, ListContext};
use super::token::{Token, TokenKind};
use super::{PResult, ParserState};

fn is_ele_for_tuple_ele_tys_and_ty_args(s: &mut ParserState) -> bool {
    s.token.kind == TokenKind::Comma || s.is_start_of_ty()
}

#[derive(Copy, Clone)]
struct TupleElementTypes;

impl ListContext for TupleElementTypes {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        is_ele_for_tuple_ele_tys_and_ty_args(s)
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        s.token.kind == TokenKind::RBracket
    }
}

#[derive(Copy, Clone)]
struct TypeArguments;

impl ListContext for TypeArguments {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        is_ele_for_tuple_ele_tys_and_ty_args(s)
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        s.token.kind != TokenKind::Comma
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    fn should_parse_ret_ty(&mut self, is_colon: bool, is_ty: bool) -> PResult<bool> {
        if !is_colon {
            self.expect(TokenKind::EqGreater)?;
            Ok(true)
        } else if self.parse_optional(TokenKind::Colon).is_some() {
            Ok(true)
        } else if is_ty && self.token.kind == TokenKind::EqGreater {
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
            self.parse_ty_or_ty_pred().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    pub(super) fn parse_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if self.is_start_of_fn_or_ctor_ty() {
            self.parse_fn_or_ctor_ty()
        } else {
            let start = self.token.start();
            let ty = self.parse_union_ty()?;
            if !self.has_preceding_line_break() && self.parse_optional(TokenKind::Extends).is_some()
            {
                let id = self.next_node_id();
                self.parent_map.r#override(ty.id(), id);
                let extends_ty = self.with_parent(id, Self::parse_ty)?;
                self.expect(TokenKind::Question)?;
                let true_ty = self.with_parent(id, Self::parse_ty)?;
                self.expect(TokenKind::Colon)?;
                let false_ty = self.with_parent(id, Self::parse_ty)?;
                let ty = self.alloc(ast::CondTy {
                    id,
                    span: self.new_span(start as usize, self.pos),
                    check_ty: ty,
                    extends_ty,
                    true_ty,
                    false_ty,
                });
                self.insert_map(id, ast::Node::CondTy(ty));
                let ty = self.alloc(ast::Ty {
                    kind: ast::TyKind::Cond(ty),
                });
                Ok(ty)
            } else {
                Ok(ty)
            }
        }
    }

    fn parse_union_or_intersection_ty(
        &mut self,
        parse_constituent_type: impl FnOnce(&mut Self) -> PResult<&'cx ast::Ty<'cx>>,
    ) -> PResult<&'cx ast::Ty<'cx>> {
        // let start = self.token.start();
        parse_constituent_type(self)
    }

    fn parse_intersection_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_union_or_intersection_ty(Self::parse_ty_op)
    }

    fn parse_union_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_union_or_intersection_ty(Self::parse_intersection_ty)
    }

    pub(super) fn parse_ty_or_ty_pred(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_ty()
    }

    fn parse_arrow_fn_ret_type(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::EqGreater).is_some() {
            self.parse_ty_or_ty_pred().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_fn_or_ctor_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        // let ty_params = self.with_parent(id, Self::parse_ty_params);
        let params = self.with_parent(id, Self::parse_params)?;
        let ret_ty = self
            .with_parent(id, Self::parse_arrow_fn_ret_type)?
            .unwrap();
        let fn_ty = self.alloc(ast::FnTy {
            id,
            span: self.new_span(start as usize, self.pos),
            params,
            ret_ty,
        });
        self.insert_map(id, ast::Node::FnTy(fn_ty));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Fn(fn_ty),
        });
        Ok(ty)
    }

    pub(super) fn parse_ty_anno(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon).is_some() {
            self.parse_ty().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_ty_op(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.parse_prefix_ty()
    }

    fn parse_prefix_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let ty = self.parse_non_array_ty()?;
        match self.token.kind {
            TokenKind::LBracket => {
                let id = self.next_node_id();
                self.expect(TokenKind::LBracket)?;
                self.parent_map.r#override(ty.id(), id);
                if self.is_start_of_ty() {
                    let index_ty = self.with_parent(id, Self::parse_ty)?;
                    self.expect(TokenKind::RBracket)?;
                    self.parent_map.r#override(ty.id(), id);
                    let kind = self.alloc(ast::IndexedAccessTy {
                        id,
                        span: self.new_span(start as usize, self.pos),
                        ty,
                        index_ty,
                    });
                    self.insert_map(id, ast::Node::IndexedAccessTy(kind));
                    let ty = self.alloc(ast::Ty {
                        kind: ast::TyKind::IndexedAccess(kind),
                    });
                    Ok(ty)
                } else {
                    self.expect(TokenKind::RBracket)?;
                    let kind = self.alloc(ast::ArrayTy {
                        id,
                        span: self.new_span(ty.span().lo as usize, self.pos),
                        ele: ty,
                    });
                    self.insert_map(id, ast::Node::ArrayTy(kind));
                    let ty = self.alloc(ast::Ty {
                        kind: ast::TyKind::Array(kind),
                    });
                    Ok(ty)
                }
            }
            _ => Ok(ty),
        }
    }

    fn parse_entity_name(&mut self) -> PResult<&'cx ast::Ident> {
        let name = self.parse_ident_name()?;
        while self.parse_optional(TokenKind::Dot).is_some() {
            todo!()
        }
        Ok(name)
    }

    fn parse_ty_args_of_ty_reference(&mut self) -> PResult<Option<ast::Tys<'cx>>> {
        if !self.has_preceding_line_break() && self.re_scan_less() == TokenKind::Less {
            Ok(Some(self.parse_bracketed_list(
                TypeArguments,
                TokenKind::Less,
                Self::parse_ty,
                TokenKind::Great,
            )?))
        } else {
            Ok(None)
        }
    }

    fn parse_entity_name_of_ty_reference(&mut self) -> PResult<&'cx ast::ReferTy<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let name = self.with_parent(id, Self::parse_entity_name)?;
        let args = self.with_parent(id, Self::parse_ty_args_of_ty_reference)?;
        let ty = self.alloc(ast::ReferTy {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            args,
        });
        self.insert_map(id, ast::Node::ReferTy(ty));
        Ok(ty)
    }

    fn parse_ty_reference(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let name = self.parse_entity_name_of_ty_reference()?;
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Refer(name),
        });
        Ok(ty)
    }

    fn parse_keyword_and_not_dot(&mut self) -> PResult<Option<Token>> {
        let token = self.parse_token_node();
        Ok((self.token.kind != TokenKind::Dot).then(|| token))
    }

    fn parse_non_array_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        use TokenKind::*;
        match self.token.kind {
            True | False => {
                todo!()
            }
            Number | String => {
                let token_val = self.token_value.unwrap();
                if let Some(node) = self.try_parse(Self::parse_keyword_and_not_dot)? {
                    let ty = match node.kind {
                        Number => {
                            let val = token_val.number();
                            let lit = self.create_lit(val, self.token.span);
                            self.insert_map(lit.id, ast::Node::NumLit(lit));
                            self.alloc(ast::Ty {
                                kind: ast::TyKind::NumLit(lit),
                            })
                        }
                        String => {
                            let val = token_val.ident();
                            let lit = self.create_lit(val, self.token.span);
                            self.insert_map(lit.id, ast::Node::StringLit(lit));
                            self.alloc(ast::Ty {
                                kind: ast::TyKind::StringLit(lit),
                            })
                        }
                        _ => unreachable!(),
                    };
                    Ok(ty)
                } else {
                    self.parse_ty_reference()
                }
            }
            LParen => self.parse_paren_ty(),
            LBrace => {
                if self
                    .lookahead(Self::is_start_of_mapped_ty)
                    .unwrap_or_default()
                {
                    todo!()
                } else {
                    self.parse_ty_lit()
                }
            }
            LBracket => self.parse_tuple_ty(),
            Minus => {
                if self.lookahead(Self::next_token_is_numeric_or_big_int_literal) {
                    self.next_token();
                    let lit = self.parse_num_lit(self.number_token(), true);
                    let ty = self.alloc(ast::Ty {
                        kind: ast::TyKind::NumLit(lit),
                    });
                    Ok(ty)
                } else {
                    todo!()
                }
            }
            _ => self.parse_ty_reference(),
        }
    }

    fn parse_tuple_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let tys = self.with_parent(id, |this| {
            this.parse_bracketed_list(
                TupleElementTypes,
                TokenKind::LBracket,
                Self::parse_tuple_ele_name_or_tuple_ele_ty,
                TokenKind::RBracket,
            )
        })?;
        let ty = self.alloc(ast::TupleTy {
            id,
            span: self.new_span(start as usize, self.pos),
            tys,
        });
        self.insert_map(id, ast::Node::TupleTy(ty));
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Tuple(ty),
        });
        Ok(ty)
    }

    fn parse_tuple_ele_name_or_tuple_ele_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if self.lookahead(Self::is_tuple_ele_name) {
            todo!()
        } else {
            self.parse_tuple_ele_ty()
        }
    }

    fn parse_tuple_ele_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if self.parse_optional(TokenKind::DotDotDot).is_some() {
            let pos = self.token.start();
            let id = self.next_node_id();
            let ty = self.with_parent(id, Self::parse_ty)?;
            let ty = self.alloc(ast::RestTy {
                id,
                span: self.new_span(pos as usize, self.pos),
                ty,
            });
            self.insert_map(id, ast::Node::RestTy(ty));
            let ty = self.alloc(ast::Ty {
                kind: ast::TyKind::Rest(ty),
            });
            return Ok(ty);
        } else {
            self.parse_ty()
        }
    }

    fn parse_ty_lit(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let id = self.next_node_id();
        let members = self.with_parent(id, Self::parse_object_ty_members)?;
        let kind = self.alloc(ast::LitTy {
            id,
            span: self.new_span(start as usize, self.pos),
            members,
        });
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Lit(kind),
        });
        self.insert_map(id, ast::Node::LitTy(kind));
        Ok(ty)
    }

    fn parse_paren_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.expect(TokenKind::LParen)?;
        let ty = self.parse_ty();
        self.expect(TokenKind::RParen)?;
        ty
    }

    pub(super) fn parse_expr_with_ty_args(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let expr = self.parse_left_hand_side_expr();
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::ExprWithArg(expr),
        });
        Ok(ty)
    }

    fn parse_prop_or_method_sig(&mut self) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let name = self.with_parent(id, Self::parse_prop_name)?;
        let kind = if self.token.kind == TokenKind::LParen {
            let ty_params = self.with_parent(id, Self::parse_ty_params)?;
            let params = self.with_parent(id, Self::parse_params)?;
            let ret = self.with_parent(id, |this| this.parse_ret_ty(true))?;
            let sig = self.alloc(ast::MethodSignature {
                id,
                span: self.new_span(start as usize, self.pos),
                name,
                ty_params,
                params,
                ret,
            });
            self.insert_map(id, ast::Node::MethodSignature(sig));
            ast::ObjectTyMemberKind::Method(sig)
        } else {
            let ty = self.parse_ty_anno()?;
            let sig = self.alloc(ast::PropSignature {
                id,
                span: self.new_span(start as usize, self.pos),
                name,
                ty,
            });
            self.insert_map(id, ast::Node::PropSignature(sig));
            ast::ObjectTyMemberKind::Prop(sig)
        };
        let node = self.alloc(ast::ObjectTyMember { kind });
        self.parse_ty_member_semi();
        Ok(node)
    }

    fn parse_sig_member(&mut self, is_call: bool) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();

        if !is_call {
            self.expect(TokenKind::New)?;
        }

        let ty_params = self.parse_ty_params()?;
        let params = self.parse_params()?;
        let ty = self.parse_ret_ty(true)?;
        self.parse_ty_member_semi();
        let kind = if is_call {
            let call_sig_decl = self.alloc(ast::CallSigDecl {
                id,
                span: self.new_span(start as usize, self.pos),
                ty_params,
                params,
                ty,
            });
            self.insert_map(id, ast::Node::CallSigDecl(call_sig_decl));
            ast::ObjectTyMemberKind::CallSig(call_sig_decl)
        } else {
            todo!()
        };
        Ok(self.alloc(ast::ObjectTyMember { kind }))
    }

    fn parse_ty_member(&mut self) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        if self.token.kind == TokenKind::LParen || self.token.kind == TokenKind::Less {
            self.parse_sig_member(true)
        } else if self.token.kind == TokenKind::New {
            todo!()
        } else if self.is_index_sig() {
            let id = self.next_node_id();
            let start = self.token.start() as usize;
            let decl = self.parse_index_sig_decl(id, start, None)?;
            Ok(self.alloc(ast::ObjectTyMember {
                kind: ast::ObjectTyMemberKind::IndexSig(decl),
            }))
        } else {
            self.parse_prop_or_method_sig()
        }
    }

    pub(super) fn parse_object_ty_members(&mut self) -> PResult<ast::ObjectTyMembers<'cx>> {
        self.expect(TokenKind::LBrace)?;
        let members = self.parse_list(list_ctx::TyMembers, Self::parse_ty_member);
        self.expect(TokenKind::RBrace)?;
        Ok(members)
    }
}
