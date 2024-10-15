use crate::ast::{self, Node};

use super::{token::TokenKind, PResult, ParserState};

impl<'cx, 'p> ParserState<'cx, 'p> {
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
        let kind = self.with_parent(id, |this| {
            let id = this.p.next_node_id();
            let fn_ty = this.alloc(ast::FnTy {
                id,
                span: this.new_span(start as usize, this.pos),
                params,
                ret_ty,
            });
            this.insert_map(id, Node::FnTy(fn_ty));
            fn_ty
        });
        let ty = self.alloc(ast::Ty {
            id,
            kind: ast::TyKind::Fn(kind),
        });
        self.insert_map(id, Node::Ty(ty));
        Ok(ty)
    }

    fn parse_ty_params(&mut self) {}

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
                    let array = self.with_parent(id, |this| {
                        let id = this.p.next_node_id();
                        this.p.parent_map.r#override(ty.id, id);
                        let kind = this.alloc(ast::ArrayTy {
                            id,
                            span: this.new_span(ty.span().lo as usize, this.pos),
                            ele: ty,
                        });
                        this.insert_map(id, Node::ArrayTy(kind));
                        kind
                    });
                    let ty = self.alloc(ast::Ty {
                        id,
                        kind: ast::TyKind::Array(array),
                    });
                    self.insert_map(id, Node::Ty(ty));
                    Ok(ty)
                }
            }
            _ => Ok(ty),
        }
    }

    fn parse_non_array_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        match self.token.kind {
            TokenKind::True | TokenKind::False => {
                todo!()
            }
            TokenKind::Ident => {
                let id = self.p.next_node_id();
                let ident = self.with_parent(id, |this| this.create_ident(true));
                let ty = self.alloc(ast::Ty {
                    id,
                    kind: ast::TyKind::Ident(ident),
                });
                self.insert_map(id, Node::Ty(ty));
                Ok(ty)
            }
            TokenKind::LParen => self.parse_paren_ty(),
            _ => todo!(),
        }
    }

    fn parse_paren_ty(&mut self) -> PResult<&'cx ast::Ty<'cx>> {
        self.expect(TokenKind::LParen)?;
        let ty = self.parse_ty();
        self.expect(TokenKind::RParen)?;
        ty
    }
}
