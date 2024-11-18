use rts_span::Span;

use crate::ast::Modifiers;

use super::ast;
use super::errors;
use super::list_ctx::{self, ListContext};
use super::token::TokenKind;
use super::{PResult, ParserState};

pub(super) trait ClassLike<'cx, 'p> {
    type Node;
    fn parse_name(&self, state: &mut ParserState<'cx, 'p>) -> PResult<Option<&'cx ast::Ident>>;
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ImplementsClause<'cx>>,
        eles: &'cx ast::ClassEles<'cx>,
    ) -> Self::Node;
}

pub(super) struct ParseClassDecl;
impl<'cx, 'a, 'p> ClassLike<'cx, 'p> for ParseClassDecl {
    type Node = &'cx ast::ClassDecl<'cx>;
    fn parse_name(&self, state: &mut ParserState<'cx, 'p>) -> PResult<Option<&'cx ast::Ident>> {
        state.parse_ident_name().map(|n| Some(n))
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ImplementsClause<'cx>>,
        eles: &'cx ast::ClassEles<'cx>,
    ) -> Self::Node {
        let name = name.unwrap();
        let decl = state.alloc(ast::ClassDecl {
            id,
            span,
            modifiers,
            name,
            ty_params,
            extends,
            implements,
            eles,
        });
        state.insert_map(decl.id, ast::Node::ClassDecl(decl));
        decl
    }
}

pub(super) struct ParseClassExpr;
impl<'cx, 'a, 'p> ClassLike<'cx, 'p> for ParseClassExpr {
    type Node = &'cx ast::ClassExpr<'cx>;
    fn parse_name(&self, state: &mut ParserState<'cx, 'p>) -> PResult<Option<&'cx ast::Ident>> {
        Ok(
            (state.token.kind.is_binding_ident() && !state.is_implements_clause())
                .then(|| state.parse_binding_ident()),
        )
    }
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ImplementsClause<'cx>>,
        eles: &'cx ast::ClassEles<'cx>,
    ) -> Self::Node {
        assert!(modifiers.is_none());
        let expr = state.alloc(ast::ClassExpr {
            id,
            span,
            name,
            ty_params,
            extends,
            implements,
            eles,
        });
        state.insert_map(expr.id, ast::Node::ClassExpr(expr));
        expr
    }
}

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_class_decl_or_expr<Node>(
        &mut self,
        mode: impl ClassLike<'cx, 'p, Node = Node>,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<Node> {
        use TokenKind::*;
        let start = self.token.start();
        self.expect(Class)?;
        let id = self.p.next_node_id();
        let name = self.with_parent(id, |this| mode.parse_name(this))?;
        let ty_params = self.with_parent(id, Self::parse_ty_params)?;
        let extends = self.with_parent(id, Self::parse_class_extends_clause)?;
        let implements = self.with_parent(id, Self::parse_implements_clause)?;
        let eles = self.with_parent(id, Self::parse_class_members)?;
        let span = self.new_span(start as usize, eles.span.hi as usize);
        Ok(mode.finish(
            self, id, span, modifiers, name, ty_params, extends, implements, eles,
        ))
    }

    fn parse_class_extends_clause(&mut self) -> PResult<Option<&'cx ast::ClassExtendsClause<'cx>>> {
        if self.token.kind == TokenKind::Extends {
            let id = self.p.next_node_id();
            let start = self.token.start();
            self.next_token();
            let mut is_first = true;
            let mut extra_comma_span = None;
            let mut ele = None;
            let mut last_ele_span = None;
            loop {
                if list_ctx::HeritageClause.is_ele(self) {
                    let e = self.parse_left_hand_side_expr();
                    if is_first {
                        ele = Some(e);
                    } else {
                        last_ele_span = Some(e.span())
                    }
                    if list_ctx::HeritageClause.is_closing(self) {
                        break;
                    }
                    let span = self.token.span;
                    if self.parse_optional(TokenKind::Comma).is_some() {
                        if is_first {
                            extra_comma_span = Some(span);
                        }
                        is_first = false;
                        continue;
                    }
                    unreachable!()
                }
                if list_ctx::HeritageClause.is_closing(self) {
                    break;
                }
            }
            let ele = ele.unwrap();
            if let Some(extra_comma_span) = extra_comma_span {
                let lo = ele.span().hi;
                assert_eq!(
                    self.input[lo as usize], b',',
                    "`parse_delimited_list` ensure it must be comma."
                );
                let hi = last_ele_span
                    .map(|span| span.hi)
                    .unwrap_or_else(|| extra_comma_span.hi);
                let extra_extends = last_ele_span
                    .is_some()
                    .then(|| self.new_span(lo as usize, hi as usize));
                let error = errors::ClassesCanOnlyExtendASingleClass {
                    span: self.new_span(lo as usize, lo as usize),
                    extra_extends,
                };
                self.push_error(self.module_id, Box::new(error));
            }
            let clause = self.alloc(ast::ClassExtendsClause {
                id,
                span: ele.span(),
                expr: ele,
            });
            self.insert_map(id, ast::Node::ClassExtendsClause(clause));
            return Ok(Some(clause));
        }
        Ok(None)
    }

    fn parse_class_prop_or_method(
        &mut self,
        id: ast::NodeID,
        start: usize,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassEle<'cx>> {
        let name = self.with_parent(id, Self::parse_prop_name)?;
        let ele = if self.token.kind == TokenKind::LParen || self.token.kind == TokenKind::Less {
            // method
            let ty_params = self.parse_ty_params()?;
            let params = self.parse_params()?;
            let ret = self.parse_ret_ty(true)?;
            let body = self.parse_fn_block()?;
            let method = self.alloc(ast::ClassMethodEle {
                id,
                span: self.new_span(start, self.pos),
                name,
                ty_params,
                params,
                ret,
                body,
            });
            self.insert_map(id, ast::Node::ClassMethodEle(method));
            self.alloc(ast::ClassEle {
                kind: ast::ClassEleKind::Method(method),
            })
        } else {
            // prop
            let ty = self.with_parent(id, Self::parse_ty_anno)?;
            let init = self.parse_init();
            let prop = self.alloc(ast::ClassPropEle {
                id,
                span: self.new_span(start, self.pos),
                modifiers,
                name,
                ty,
                init,
            });
            self.insert_map(id, ast::Node::ClassPropEle(prop));
            self.parse_semi_after_prop_name();
            self.alloc(ast::ClassEle {
                kind: ast::ClassEleKind::Prop(prop),
            })
        };
        Ok(ele)
    }

    fn parse_index_sig_decl(
        &mut self,
        id: ast::NodeID,
        start: usize,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::IndexSigDecl<'cx>> {
        let params = self.parse_bracketed_list(
            TokenKind::LBracket,
            list_ctx::Params,
            Self::parse_param,
            TokenKind::RBracket,
        )?;
        let Some(ty) = self.parse_ty_anno()? else {
            todo!("error handler")
        };
        self.parse_ty_member_semi();
        let sig = self.alloc(ast::IndexSigDecl {
            id,
            span: self.new_span(start, self.pos),
            modifiers,
            params,
            ty,
        });
        self.insert_map(id, ast::Node::IndexSigDecl(sig));
        Ok(sig)
    }

    fn try_parse_ctor(
        &mut self,
        id: ast::NodeID,
        start: usize,
        mods: Option<&'cx Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassEle<'cx>> {
        self.try_parse(|this| {
            if this.token.kind == TokenKind::Constructor
                && this.expect(TokenKind::Constructor).is_ok()
            {
                let ty_params = this.parse_ty_params()?;
                let params = this.parse_params()?;
                let ret = this.parse_ret_ty(true)?;
                let body = this.parse_fn_block()?;
                let ctor = this.alloc(ast::ClassCtor {
                    id,
                    span: this.new_span(start, this.pos),
                    ty_params,
                    params,
                    ret,
                    body,
                });
                this.insert_map(id, ast::Node::ClassCtor(ctor));
                let ele = this.alloc(ast::ClassEle {
                    kind: ast::ClassEleKind::Ctor(ctor),
                });
                Ok(ele)
            } else {
                Err(())
            }
        })
    }

    fn parse_class_ele(&mut self) -> PResult<&'cx ast::ClassEle<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start() as usize;
        let modifiers = self.with_parent(id, Self::parse_modifiers)?;
        if self.token.kind == TokenKind::Constructor {
            if let Ok(ctor) = self.try_parse_ctor(id, start, modifiers) {
                return Ok(ctor);
            }
        }
        if self.is_index_sig() {
            let decl = self.parse_index_sig_decl(id, start, modifiers)?;
            Ok(self.alloc(ast::ClassEle {
                kind: ast::ClassEleKind::IndexSig(decl),
            }))
        } else {
            self.parse_class_prop_or_method(id, start, modifiers)
        }
    }

    fn parse_class_members(&mut self) -> PResult<&'cx ast::ClassEles<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBrace)?;
        let eles = self.parse_list(list_ctx::ClassElements, Self::parse_class_ele);
        let end = self.token.end();
        self.expect(TokenKind::RBrace)?;
        let span = self.new_span(start as usize, end as usize);
        Ok(self.alloc(ast::ClassEles { span, eles }))
    }
}
