use bolt_ts_span::Span;

use crate::ast::Modifiers;
use crate::keyword;

use super::ast;
use super::errors;
use super::list_ctx::{self, ListContext};
use super::token::TokenKind;
use super::{PResult, ParserState};

fn is_class_ele_start(s: &mut ParserState) -> bool {
    let mut id_token = None;

    // if s.token.kind == TokenKind::At {
    //     return true;
    // }
    while s.token.kind.is_modifier_kind() {
        id_token = Some(s.token.kind);
        if s.token.kind.is_class_ele_modifier() {
            return true;
        }
        s.next_token();
    }

    if s.token.kind.is_lit_prop_name() {
        id_token = Some(s.token.kind);
        s.next_token();
    }

    if s.token.kind == TokenKind::LBracket {
        return true;
    }

    if let Some(t) = id_token {
        if !t.is_keyword() || t == TokenKind::Get || t == TokenKind::Set {
            true
        } else {
            use TokenKind::*;
            match s.token.kind {
                LParen | Less | Colon | Eq | Question | Excl => true,
                _ => s.can_parse_semi(),
            }
        }
    } else {
        false
    }
}

#[derive(Copy, Clone)]
struct ClassElementsCtx;
impl ListContext for ClassElementsCtx {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        s.lookahead(is_class_ele_start) || s.token.kind == TokenKind::Semi
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

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
        elems: &'cx ast::ClassElems<'cx>,
    ) -> Self::Node;
}

pub(super) struct ParseClassDecl;
impl<'cx, 'p> ClassLike<'cx, 'p> for ParseClassDecl {
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
        elems: &'cx ast::ClassElems<'cx>,
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
            elems,
        });
        state.insert_map(decl.id, ast::Node::ClassDecl(decl));
        decl
    }
}

pub(super) struct ParseClassExpr;
impl<'cx, 'p> ClassLike<'cx, 'p> for ParseClassExpr {
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
        elems: &'cx ast::ClassElems<'cx>,
    ) -> Self::Node {
        assert!(modifiers.is_none());
        let expr = state.alloc(ast::ClassExpr {
            id,
            span,
            name,
            ty_params,
            extends,
            implements,
            elems,
        });
        state.insert_map(expr.id, ast::Node::ClassExpr(expr));
        expr
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_class_decl_or_expr<Node>(
        &mut self,
        mode: impl ClassLike<'cx, 'p, Node = Node>,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<Node> {
        use TokenKind::*;
        let start = self.token.start();
        self.expect(Class)?;
        let id = self.next_node_id();
        let name = self.with_parent(id, |this| mode.parse_name(this))?;
        let ty_params = self.with_parent(id, Self::parse_ty_params)?;
        let extends = self.with_parent(id, Self::parse_class_extends_clause)?;
        let implements = self.with_parent(id, Self::parse_implements_clause)?;
        let elems = self.with_parent(id, Self::parse_class_members)?;
        let span = self.new_span(start as usize, elems.span.hi as usize);
        Ok(mode.finish(
            self, id, span, modifiers, name, ty_params, extends, implements, elems,
        ))
    }

    fn parse_class_extends_clause(&mut self) -> PResult<Option<&'cx ast::ClassExtendsClause<'cx>>> {
        if self.token.kind == TokenKind::Extends {
            let id = self.next_node_id();
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
                self.push_error(Box::new(error));
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
            let ty_params = self.with_parent(id, Self::parse_ty_params)?;
            let params = self.parse_params()?;
            let ret = self.parse_ret_ty(true)?;
            let body = self.parse_fn_block()?;
            let method = self.alloc(ast::ClassMethodEle {
                id,
                span: self.new_span(start, self.pos),
                modifiers,
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

    fn parse_ctor_name(&mut self) -> bool {
        use TokenKind::*;
        let t = self.token;
        if t.kind == Constructor {
            self.expect(Constructor).is_ok()
        } else if t.kind == String
            && self.lookahead(|this| {
                this.next_token();
                this.token.kind == LParen
            })
        {
            let val = self.string_token();
            self.try_parse(|this| {
                let lit = this.parse_string_lit(val);
                if lit.val == keyword::KW_CONSTRUCTOR {
                    true
                } else {
                    false
                }
            })
        } else {
            false
        }
    }

    fn try_parse_ctor(
        &mut self,
        id: ast::NodeID,
        start: usize,
        mods: Option<&'cx Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassEle<'cx>> {
        self.try_parse(|this| {
            if this.parse_ctor_name() {
                let ty_params = this.with_parent(id, Self::parse_ty_params)?;
                let params = this.with_parent(id, Self::parse_params)?;
                let ret = this.with_parent(id, |this| this.parse_ret_ty(true))?;
                let body = this.with_parent(id, Self::parse_fn_block)?;
                let ctor = this.alloc(ast::ClassCtor {
                    id,
                    span: this.new_span(start, this.full_start_pos),
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

    fn parse_accessor_decl(
        &mut self,
        id: ast::NodeID,
        start: usize,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        t: TokenKind,
    ) -> PResult<&'cx ast::ClassEle<'cx>> {
        let is_getter = t == TokenKind::Get;
        assert!(is_getter || t == TokenKind::Set);
        let name = self.with_parent(id, Self::parse_prop_name)?;
        let ty_params = self.parse_ty_params()?;
        let params = self.parse_params()?;
        let ret = self.parse_ret_ty(true)?;
        let body = self.parse_fn_block()?;
        let kind = if is_getter {
            let decl = self.alloc(ast::GetterDecl {
                id,
                span: self.new_span(start, self.pos),
                name,
                ret,
                body,
            });
            self.insert_map(id, ast::Node::GetterDecl(decl));
            ast::ClassEleKind::Getter(decl)
        } else {
            let decl = self.alloc(ast::SetterDecl {
                id,
                span: self.new_span(start, self.pos),
                name,
                params,
                body,
            });
            self.insert_map(id, ast::Node::SetterDecl(decl));
            ast::ClassEleKind::Setter(decl)
        };
        let ele = self.alloc(ast::ClassEle { kind });
        Ok(ele)
    }

    fn parse_class_ele(&mut self) -> PResult<&'cx ast::ClassEle<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start() as usize;
        let modifiers = self.with_parent(id, Self::parse_modifiers)?;
        if self.parse_contextual_modifier(TokenKind::Get) {
            return self.parse_accessor_decl(id, start, modifiers, TokenKind::Get);
        } else if self.parse_contextual_modifier(TokenKind::Set) {
            return self.parse_accessor_decl(id, start, modifiers, TokenKind::Set);
        }

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

    fn parse_class_members(&mut self) -> PResult<&'cx ast::ClassElems<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBrace)?;
        let elems = self.parse_list(ClassElementsCtx, Self::parse_class_ele);
        let end = self.token.end();
        self.expect(TokenKind::RBrace)?;
        let span = self.new_span(start as usize, end as usize);
        Ok(self.alloc(ast::ClassElems { span, elems }))
    }
}
