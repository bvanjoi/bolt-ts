use super::ast::{self, Node};
use super::list_ctx::{ListContext, TyMembers};
use super::token::TokenKind;
use super::{PResult, ParserState};

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

    fn parse_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        if self.is_start_of_fn_or_ctor_ty() {
            self.parse_fn_or_ctor_ty()
        } else {
            self.parse_union_ty()
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
        let id = self.p.next_node_id();
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
        self.insert_map(id, Node::FnTy(fn_ty));
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
        let ty = self.parse_non_array_ty()?;
        match self.token.kind {
            TokenKind::LBracket => {
                let id = self.p.next_node_id();
                self.expect(TokenKind::LBracket)?;
                if self.token.kind.is_start_of_type() {
                    // let index_ty = self.parse_ty()?;
                    self.expect(TokenKind::RBracket)?;
                    todo!()
                } else {
                    self.expect(TokenKind::RBracket)?;
                    self.p.parent_map.r#override(ty.id(), id);
                    let kind = self.alloc(ast::ArrayTy {
                        id,
                        span: self.new_span(ty.span().lo as usize, self.pos),
                        ele: ty,
                    });
                    self.insert_map(id, Node::ArrayTy(kind));
                    let ty = self.alloc(ast::Ty {
                        kind: ast::TyKind::Array(kind),
                    });
                    Ok(ty)
                }
            }
            _ => Ok(ty),
        }
    }

    fn parse_non_array_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        use TokenKind::*;
        match self.token.kind {
            True | False => {
                todo!()
            }
            Ident => {
                let ident = self.create_ident(true);
                let ty = self.alloc(ast::Ty {
                    kind: ast::TyKind::Ident(ident),
                });
                Ok(ty)
            }
            LParen => self.parse_paren_ty(),
            LBrace => self.parse_ty_lit(),
            _ => todo!(),
        }
    }

    fn parse_ty_lit(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        let start = self.token.start();
        let id = self.p.next_node_id();
        let members = self.with_parent(id, Self::parse_object_ty_members)?;
        let kind = self.alloc(ast::LitTy {
            id,
            span: self.new_span(start as usize, self.pos),
            members,
        });
        let ty = self.alloc(ast::Ty {
            kind: ast::TyKind::Lit(kind),
        });
        self.insert_map(id, Node::LitTy(kind));
        Ok(ty)
    }

    fn parse_paren_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.expect(TokenKind::LParen)?;
        let ty = self.parse_ty();
        self.expect(TokenKind::RParen)?;
        ty
    }

    pub(super) fn parse_expr_with_ty_args(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        Ok(self.parse_left_hand_side_expr())
    }

    fn parse_ty_member_semi(&mut self) {
        if self.parse_optional(TokenKind::Semi).is_some() {
            return;
        }
        self.parse_semi();
    }

    fn parse_prop_or_method_sig(&mut self) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        let id = self.p.next_node_id();
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
            ast::ObjectTyMemberKind::Method(sig)
        } else {
            let ty = self.parse_ty_anno()?;
            let sig = self.alloc(ast::PropSignature {
                id,
                span: self.new_span(start as usize, self.pos),
                name,
                ty,
            });
            ast::ObjectTyMemberKind::Prop(sig)
        };
        let node = self.alloc(ast::ObjectTyMember { kind });
        self.parse_ty_member_semi();
        Ok(node)
    }

    fn parse_ty_member(&mut self) -> PResult<&'cx ast::ObjectTyMember<'cx>> {
        if self.token.kind == TokenKind::LParen {
            todo!()
        } else if self.token.kind == TokenKind::New {
            todo!()
        }
        self.parse_prop_or_method_sig()
    }

    pub(super) fn parse_object_ty_members(&mut self) -> PResult<ast::ObjectTyMembers<'cx>> {
        self.expect(TokenKind::LBrace)?;
        let members = self.parse_list(
            TyMembers::is_ele,
            Self::parse_ty_member,
            TyMembers::is_closing,
        );
        self.expect(TokenKind::RBrace)?;
        Ok(members)
    }
}
