use bolt_ts_ast::TokenKind;
use bolt_ts_ast::{self as ast};
use bolt_ts_span::Span;

use super::errors;
use super::{PResult, ParserState};
use crate::parsing_ctx::{ParseContext, ParsingContext};
use crate::{SignatureFlags, keyword};

pub(super) fn is_class_ele_start(s: &mut ParserState) -> bool {
    let mut id_token = None;

    if s.token.kind == TokenKind::At {
        return true;
    }

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
        debug_assert!(self.token.kind == TokenKind::Class);
        let start = self.token.start();
        let old_in_strict_mode = self.in_strict_mode;
        self.in_strict_mode = true;
        self.next_token(); // consume `class`
        let name = self.parse_name_of_class_decl_or_expr();
        if let Some(name) = name {
            self.check_contextual_ident(name);
        }
        let ty_params = self.parse_ty_params();
        let mut extends = self.parse_class_extends_clause()?;
        let mut implements = self.parse_implements_clause();
        loop {
            if !matches!(self.token.kind, TokenKind::Implements | TokenKind::Extends) {
                break;
            }
            if self.token.kind == TokenKind::Extends
                && let Some(origin) = extends
            {
                let error = errors::ClauseAlreadySeen {
                    span: self.token.span,
                    kind: errors::ClauseKind::Extends,
                    origin: origin.span,
                };
                self.push_error(Box::new(error));
            } else if self.token.kind == TokenKind::Implements
                && let Some(origin) = implements
            {
                let error = errors::ClauseAlreadySeen {
                    span: self.token.span,
                    kind: errors::ClauseKind::Implements,
                    origin: origin.span,
                };
                self.push_error(Box::new(error));
            }

            let e = self.parse_class_extends_clause()?;
            if extends.is_none() {
                extends = e;
                if let Some(extends) = extends
                    && let Some(implements) = implements
                {
                    let error = errors::ExtendsClauseMustPrecedeImplementsClause {
                        extends_span: extends.span,
                        implements_span: implements.span,
                    };
                    self.push_error(Box::new(error));
                }
            }
            let i = self.parse_implements_clause();
            if implements.is_none() {
                implements = i;
            }
        }

        let elems = self.parse_class_members()?;
        self.in_strict_mode = old_in_strict_mode;
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
            let mut e = None;
            let mut last_expr_span = None;
            loop {
                if self.is_list_element(ParsingContext::HERITAGE_CLAUSE_ELEMENT, false) {
                    let start_pos = self.token.start();
                    let expr = self.parse_left_hand_side_expr_or_higher()?;
                    let expr = if let ast::ExprKind::ExprWithTyArgs(expr) = expr.kind {
                        expr
                    } else {
                        let ty_arguments = self.try_parse_ty_args();
                        let id = self.next_node_id();
                        let expr = self.alloc(ast::ExprWithTyArgs {
                            id,
                            span: self.new_span(start_pos),
                            expr,
                            ty_args: ty_arguments,
                        });
                        self.nodes.insert(id, ast::Node::ExprWithTyArgs(expr));
                        expr
                    };
                    if is_first {
                        e = Some(expr);
                    } else {
                        last_expr_span = Some(expr.span);
                    }
                    if self.is_list_terminator(ParsingContext::HERITAGE_CLAUSE_ELEMENT) {
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

                    if start_pos == self.token.start() {
                        self.next_token();
                    }

                    continue;
                }
                if self.is_list_terminator(ParsingContext::HERITAGE_CLAUSE_ELEMENT)
                    || self.abort_parsing_list_or_move_to_next_token(
                        ParsingContext::HERITAGE_CLAUSE_ELEMENT,
                    )
                {
                    break;
                }
            }
            let expr = e.unwrap();
            if let Some(extra_comma_span) = extra_comma_span {
                let lo = expr.span.hi();
                assert_eq!(
                    self.input[lo as usize], b',',
                    "`parse_delimited_list` ensure it must be comma."
                );
                let hi = last_expr_span
                    .map(|span| span.hi())
                    .unwrap_or_else(|| extra_comma_span.hi());
                let extra_extends = last_expr_span
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
                expr_with_ty_args: expr,
            });
            self.nodes.insert(id, ast::Node::ClassExtendsClause(clause));
            return Ok(Some(clause));
        }
        Ok(None)
    }

    fn parse_class_prop_or_method(
        &mut self,
        start: u32,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassElem<'cx>> {
        let name = self.parse_prop_name(true);
        let ele = if matches!(self.token.kind, TokenKind::LParen | TokenKind::Less) {
            // method
            let ty_params = self.parse_ty_params();
            let params = self.parse_params();
            let ty = self.parse_ret_ty(true)?;
            let flags = if modifiers.is_some_and(|m| m.flags.contains(ast::ModifierKind::Async)) {
                SignatureFlags::ASYNC.union(SignatureFlags::AWAIT)
            } else {
                SignatureFlags::empty()
            };
            let body = self.parse_fn_block_or_semi(flags);
            let method =
                self.create_class_method_elem(start, modifiers, name, ty_params, params, ty, body);
            self.alloc(ast::ClassElem {
                kind: ast::ClassElemKind::Method(method),
            })
        } else {
            // prop
            if let ast::PropNameKind::StringLit { raw, .. } = name.kind
                && raw.val == keyword::KW_CONSTRUCTOR
            {
                let error = errors::ClassesMayNotHaveAFieldNamedConstructor { span: name.span() };
                self.push_error(Box::new(error));
            }
            self.do_inside_of_parse_context(ParseContext::CLASS_FIELD_DEFINITION, |this| {
                let excl = if !this.has_preceding_line_break() {
                    this.parse_optional(TokenKind::Excl)
                } else {
                    None
                };
                let ty = this.parse_ty_anno()?;
                let init = this.parse_init()?;
                if let Some(init) = init
                    && modifiers.is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Ambient))
                {
                    let error =
                        errors::InitializersAreNotAllowedInAmbientContexts { span: init.span() };
                    this.push_error(Box::new(error));
                }
                let prop = this.create_class_prop_elem(start, modifiers, name, ty, init, excl);
                this.parse_semi_after_prop_name();
                Ok(this.alloc(ast::ClassElem {
                    kind: ast::ClassElemKind::Prop(prop),
                }))
            })?
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
                this.p().next_token();
                this.p().token.kind == LParen
            })
        {
            self.try_parse(|this| {
                let lit = this.p().parse_string_lit();
                lit.val == keyword::KW_CONSTRUCTOR
            })
        } else {
            false
        }
    }

    fn try_parse_ctor(
        &mut self,
        start: u32,
        mods: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<Option<&'cx ast::ClassElem<'cx>>> {
        self.try_parse(|this| {
            let name_span = this.p().token.span;
            if this.p().parse_ctor_name() {
                let ty_params = this.p().parse_ty_params();
                let params = this.p().parse_params();
                this.p().check_params(params, true);
                let ret = this.p().parse_ret_ty(true)?;
                let flags = if mods.is_some_and(|m| m.flags.contains(ast::ModifierKind::Async)) {
                    SignatureFlags::ASYNC.union(SignatureFlags::AWAIT)
                } else {
                    SignatureFlags::empty()
                };
                let body = this.p().parse_fn_block_or_semi(flags);
                let ctor = this
                    .p()
                    .create_class_ctor(start, mods, ty_params, name_span, params, ret, body);
                let ele = this.p().alloc(ast::ClassElem {
                    kind: ast::ClassElemKind::Ctor(ctor),
                });
                Ok(Some(ele))
            } else {
                Ok(None)
            }
        })
    }

    fn parse_class_ele(&mut self) -> PResult<&'cx ast::ClassElem<'cx>> {
        let start = self.token.start();

        if self.token.kind == TokenKind::Static
            && self.lookahead(|l| {
                l.p().next_token();
                l.p().token.kind == TokenKind::LBrace
            })
        {
            return self.parse_class_static_block_decl();
        }

        let modifiers = self.parse_modifiers::<false, true>(true);

        if let Some(ms) = modifiers {
            for m in ms.list {
                let error = match m.kind {
                    ast::ModifierKind::Const => {
                        Box::new(errors::AClassMemberCannotHaveTheModifierKeyword {
                            span: m.span,
                            modifier: m.kind,
                        }) as Box<_>
                    }
                    ast::ModifierKind::Export => {
                        Box::new(errors::ModifierCannotAppearOnClassElementsOfThisKind {
                            span: m.span,
                            modifier: m.kind,
                        }) as Box<_>
                    }
                    _ => continue,
                };
                self.push_error(error);
            }
        }

        if self.parse_contextual_modifier(TokenKind::Get) {
            let ambient =
                modifiers.is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Abstract));
            let decl = self.parse_getter_accessor_decl(
                start,
                modifiers,
                ambient,
                SignatureFlags::empty(),
            )?;
            Ok(self.alloc(ast::ClassElem {
                kind: ast::ClassElemKind::Getter(decl),
            }))
        } else if self.parse_contextual_modifier(TokenKind::Set) {
            let ambient =
                modifiers.is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Abstract));
            let decl = self.parse_setter_accessor_decl(
                start,
                modifiers,
                ambient,
                SignatureFlags::empty(),
            )?;
            Ok(self.alloc(ast::ClassElem {
                kind: ast::ClassElemKind::Setter(decl),
            }))
        } else if matches!(self.token.kind, TokenKind::Constructor | TokenKind::String)
            && let Ok(Some(ctor)) = self.try_parse_ctor(start, modifiers)
        {
            Ok(ctor)
        } else if self.is_index_sig() {
            if let Some(ms) = modifiers {
                for m in ms.list {
                    use ast::ModifierKind;
                    if !matches!(m.kind, ModifierKind::Readonly | ModifierKind::Static) {
                        self.push_error(Box::new(errors::ModifierCannotAppearOnAnIndexSignature {
                            span: m.span,
                            kind: m.kind,
                        }));
                    }
                }
            }
            let decl = self.parse_index_sig_decl(start, modifiers)?;
            Ok(self.alloc(ast::ClassElem {
                kind: ast::ClassElemKind::IndexSig(decl),
            }))
        } else {
            self.parse_class_prop_or_method(start, modifiers)
        }
    }

    fn parse_class_static_block_decl(&mut self) -> PResult<&'cx ast::ClassElem<'cx>> {
        debug_assert!(self.token.kind == TokenKind::Static);
        let start = self.token.start() as usize;
        self.next_token(); // consume `static`
        let body =
            self.do_inside_of_parse_context(ParseContext::CLASS_STATIC_BLOCK, Self::parse_block);
        let block = self.create_class_static_block_decl(start as u32, body);
        Ok(self.alloc(ast::ClassElem {
            kind: ast::ClassElemKind::StaticBlockDecl(block),
        }))
    }

    fn parse_class_members(&mut self) -> PResult<&'cx ast::ClassElems<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBrace);
        let elems = self.parse_list(ParsingContext::CLASS_MEMBERS, Self::parse_class_ele);
        let end = self.token.end();
        self.expect(TokenKind::RBrace);
        let span = Span::new(start, end, self.module_id);
        Ok(self.alloc(ast::ClassElems { span, list: elems }))
    }
}
