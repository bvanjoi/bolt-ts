use bolt_ts_ast as ast;
use bolt_ts_ast::TokenKind;
use bolt_ts_span::Span;

use super::errors;
use super::list_ctx::{self, ListContext};
use super::{PResult, ParserState};
use crate::keyword;

fn is_class_ele_start(s: &mut ParserState) -> bool {
    let mut id_token = None;

    // TODO: decorator
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
        if !t.is_keyword() || matches!(t, TokenKind::Get | TokenKind::Set) {
            true
        } else {
            use bolt_ts_ast::TokenKind::*;
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
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        s.lookahead(is_class_ele_start) || s.token.kind == TokenKind::Semi
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

pub(super) trait ClassLike<'cx, 'p> {
    type Node;
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ClassImplementsClause<'cx>>,
        elems: &'cx ast::ClassElems<'cx>,
    ) -> Self::Node;
}

pub(super) struct ParseClassDecl;
impl<'cx, 'p> ClassLike<'cx, 'p> for ParseClassDecl {
    type Node = &'cx ast::ClassDecl<'cx>;
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ClassImplementsClause<'cx>>,
        elems: &'cx ast::ClassElems<'cx>,
    ) -> Self::Node {
        let id = state.next_node_id();
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
        state.set_external_module_indicator_if_has_export_mod(modifiers, id);
        state.nodes.insert(decl.id, ast::Node::ClassDecl(decl));
        decl
    }
}

pub(super) struct ParseClassExpr;
impl<'cx, 'p> ClassLike<'cx, 'p> for ParseClassExpr {
    type Node = &'cx ast::ClassExpr<'cx>;
    fn finish(
        self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ClassImplementsClause<'cx>>,
        elems: &'cx ast::ClassElems<'cx>,
    ) -> Self::Node {
        assert!(modifiers.is_none());
        let id = state.next_node_id();
        let expr = state.alloc(ast::ClassExpr {
            id,
            span,
            name,
            ty_params,
            extends,
            implements,
            elems,
        });
        state.nodes.insert(expr.id, ast::Node::ClassExpr(expr));
        expr
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    fn parse_name_of_class_decl_or_expr(&mut self) -> Option<&'cx ast::Ident> {
        (self.token.kind.is_binding_ident() && !self.is_implements_clause())
            .then(|| self.parse_binding_ident())
    }
    pub(super) fn parse_class_decl_or_expr<Node>(
        &mut self,
        mode: impl ClassLike<'cx, 'p, Node = Node>,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<Node> {
        use bolt_ts_ast::TokenKind::*;
        let start = self.token.start();
        self.expect(Class);
        let name = self.parse_name_of_class_decl_or_expr();
        let ty_params = self.parse_ty_params()?;
        let mut extends = self.parse_class_extends_clause()?;
        let mut implements = self.parse_implements_clause()?;
        loop {
            if !matches!(self.token.kind, TokenKind::Implements | TokenKind::Extends) {
                break;
            }
            if self.token.kind == TokenKind::Extends {
                if let Some(origin) = extends {
                    let error = errors::ClauseAlreadySeen {
                        span: self.token.span,
                        kind: errors::ClauseKind::Extends,
                        origin: origin.span,
                    };
                    self.push_error(Box::new(error));
                }
            }
            if self.token.kind == TokenKind::Implements {
                if let Some(origin) = implements {
                    let error = errors::ClauseAlreadySeen {
                        span: self.token.span,
                        kind: errors::ClauseKind::Implements,
                        origin: origin.span,
                    };
                    self.push_error(Box::new(error));
                }
            }

            let e = self.parse_class_extends_clause()?;
            if extends.is_none() {
                extends = e;
                if let Some(extends) = extends {
                    if let Some(implements) = implements {
                        let error = errors::ExtendsClauseMustPrecedeImplementsClause {
                            extends_span: extends.span,
                            implements_span: implements.span,
                        };
                        self.push_error(Box::new(error));
                    }
                }
            }
            let i = self.parse_implements_clause()?;
            if implements.is_none() {
                implements = i;
            }
        }

        let elems = self.parse_class_members()?;
        let span = self.new_span(start);
        Ok(mode.finish(
            self, span, modifiers, name, ty_params, extends, implements, elems,
        ))
    }

    fn parse_class_extends_clause(&mut self) -> PResult<Option<&'cx ast::ClassExtendsClause<'cx>>> {
        if self.token.kind == TokenKind::Extends {
            let start = self.token.start();
            self.next_token();
            let mut is_first = true;
            let mut extra_comma_span = None;
            let mut ele = None;
            let mut ty_args = None;
            let mut last_ele_span = None;
            loop {
                if list_ctx::HeritageClause.is_ele(self, false) {
                    let e = self.parse_entity_name(true)?;
                    let args = self.try_parse_ty_args()?;
                    if is_first {
                        ele = Some(e);
                        ty_args = Some(args);
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
            let ty_args = ty_args.unwrap();
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
                    .then(|| Span::new(lo, hi, self.module_id));
                let error = errors::ClassesCanOnlyExtendASingleClass {
                    span: Span::new(lo, lo, self.module_id),
                    extra_extends,
                };
                self.push_error(Box::new(error));
            }
            let id = self.next_node_id();
            let clause = self.alloc(ast::ClassExtendsClause {
                id,
                span: self.new_span(start),
                name: ele,
                ty_args,
            });
            self.nodes.insert(id, ast::Node::ClassExtendsClause(clause));
            return Ok(Some(clause));
        }
        Ok(None)
    }

    fn parse_class_prop_or_method(
        &mut self,
        start: usize,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassElem<'cx>> {
        let name = self.parse_prop_name(false)?;
        let ele = if matches!(self.token.kind, TokenKind::LParen | TokenKind::Less) {
            // method
            let ty_params = self.parse_ty_params()?;
            let params = self.parse_params()?;
            let ty = self.parse_ret_ty(true)?;
            let body = self.parse_fn_block()?;
            let id = self.next_node_id();
            let method = self.alloc(ast::ClassMethodElem {
                id,
                span: self.new_span(start as u32),
                modifiers,
                name,
                ty_params,
                params,
                ty,
                body,
            });
            self.node_flags_map.insert(id, self.context_flags);
            self.nodes.insert(id, ast::Node::ClassMethodElem(method));
            self.alloc(ast::ClassElem {
                kind: ast::ClassEleKind::Method(method),
            })
        } else {
            // prop
            let excl = if !self.has_preceding_line_break() {
                self.parse_optional(TokenKind::Excl)
            } else {
                None
            };
            let ty = self.parse_ty_anno()?;
            let init = self.parse_init()?;
            let id = self.next_node_id();
            let prop = self.alloc(ast::ClassPropElem {
                id,
                span: self.new_span(start as u32),
                modifiers,
                name,
                ty,
                init,
                question: None,
                excl: excl.map(|e| e.span),
            });
            self.nodes.insert(id, ast::Node::ClassPropElem(prop));
            self.parse_semi_after_prop_name();
            self.alloc(ast::ClassElem {
                kind: ast::ClassEleKind::Prop(prop),
            })
        };
        Ok(ele)
    }

    fn parse_ctor_name(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        let t = self.token;
        if t.kind == Constructor {
            self.expect(Constructor)
        } else if t.kind == String
            && self.lookahead(|this| {
                this.next_token();
                this.token.kind == LParen
            })
        {
            self.try_parse(|this| {
                let lit = this.parse_string_lit();
                lit.val == keyword::KW_CONSTRUCTOR
            })
        } else {
            false
        }
    }

    fn try_parse_ctor(
        &mut self,
        start: usize,
        mods: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<Option<&'cx ast::ClassElem<'cx>>> {
        self.try_parse(|this| {
            if this.parse_ctor_name() {
                let ty_params = this.parse_ty_params()?;
                let params = this.parse_params()?;
                let ret = this.parse_ret_ty(true)?;
                let body = this.parse_fn_block()?;
                let id = this.next_node_id();
                let ctor = this.alloc(ast::ClassCtor {
                    id,
                    span: this.new_span(start as u32),
                    ty_params,
                    params,
                    ret,
                    body,
                });
                this.nodes.insert(id, ast::Node::ClassCtor(ctor));
                let ele = this.alloc(ast::ClassElem {
                    kind: ast::ClassEleKind::Ctor(ctor),
                });
                Ok(Some(ele))
            } else {
                Ok(None)
            }
        })
    }

    fn parse_accessor_decl(
        &mut self,
        start: usize,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        t: TokenKind,
    ) -> PResult<&'cx ast::ClassElem<'cx>> {
        let is_getter = t == TokenKind::Get;
        assert!(is_getter || t == TokenKind::Set);
        let kind = if is_getter {
            let decl = self.parse_getter_accessor_decl(start, modifiers, false)?;
            ast::ClassEleKind::Getter(decl)
        } else {
            let decl = self.parse_setter_accessor_decl(start, modifiers, false)?;
            ast::ClassEleKind::Setter(decl)
        };
        Ok(self.alloc(ast::ClassElem { kind }))
    }

    fn parse_class_ele(&mut self) -> PResult<&'cx ast::ClassElem<'cx>> {
        let start = self.token.start() as usize;
        let modifiers = self.parse_modifiers(true)?;
        if self.parse_contextual_modifier(TokenKind::Get) {
            return self.parse_accessor_decl(start, modifiers, TokenKind::Get);
        } else if self.parse_contextual_modifier(TokenKind::Set) {
            return self.parse_accessor_decl(start, modifiers, TokenKind::Set);
        }

        if self.token.kind == TokenKind::Constructor {
            if let Ok(Some(ctor)) = self.try_parse_ctor(start, modifiers) {
                return Ok(ctor);
            }
        }
        if self.is_index_sig() {
            let decl = self.parse_index_sig_decl(start, modifiers)?;
            Ok(self.alloc(ast::ClassElem {
                kind: ast::ClassEleKind::IndexSig(decl),
            }))
        } else {
            self.parse_class_prop_or_method(start, modifiers)
        }
    }

    fn parse_class_members(&mut self) -> PResult<&'cx ast::ClassElems<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBrace);
        let elems = self.parse_list(ClassElementsCtx, Self::parse_class_ele);
        let end = self.token.end();
        self.expect(TokenKind::RBrace);
        let span = Span::new(start, end, self.module_id);
        Ok(self.alloc(ast::ClassElems { span, elems }))
    }
}
