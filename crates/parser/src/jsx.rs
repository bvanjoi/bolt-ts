use bolt_ts_ast::{JsxTagName, NodeFlags, TokenKind, keyword};
use bolt_ts_span::Span;

use crate::{errors, parsing_ctx::ParsingContext, state::LanguageVariant};

use super::{PResult, ParserState};

#[derive(Debug)]
pub(super) enum JsxEleOrSelfClosingEleOrFrag<'cx> {
    Ele(&'cx bolt_ts_ast::JsxElem<'cx>),
    SelfClosingEle(&'cx bolt_ts_ast::JsxSelfClosingElem<'cx>),
    Frag(&'cx bolt_ts_ast::JsxFrag<'cx>),
}

impl JsxEleOrSelfClosingEleOrFrag<'_> {
    pub fn span(&self) -> Span {
        match self {
            JsxEleOrSelfClosingEleOrFrag::Ele(n) => n.span,
            JsxEleOrSelfClosingEleOrFrag::SelfClosingEle(n) => n.span,
            JsxEleOrSelfClosingEleOrFrag::Frag(n) => n.span,
        }
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_jsx_ele_or_self_closing_ele_or_frag(
        &mut self,
        in_expr_context: bool,
        top_invalid_node_position: Option<u32>,
        opening_tag: Option<bolt_ts_ast::JsxTagName<'cx>>,
        must_be_unary: bool,
    ) -> PResult<JsxEleOrSelfClosingEleOrFrag<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        // parse_jsx_opening_or_self_closing_or_opening_frag
        let result: JsxEleOrSelfClosingEleOrFrag<'cx>;
        self.expect(TokenKind::Less);
        if self.token.kind == TokenKind::Great {
            self.scan_jsx_token(true);
            let opening = self.create_jsx_opening_frag(self.new_span(start));
            let children = self.parse_jsx_children(None);
            let closing = self.parse_jsx_closing_frag(in_expr_context);
            let span = self.new_span(start);
            result = JsxEleOrSelfClosingEleOrFrag::Frag(
                self.create_jsx_frag(span, opening, children, closing),
            );
        } else {
            let tag_name = self.parse_jsx_ele_name()?;
            let ty_args = if !self.context_flags.contains(NodeFlags::JAVASCRIPT_FILE) {
                self.try_parse_ty_args()?
            } else {
                None
            };
            let attrs = self.parse_jsx_attrs();
            if self.token.kind == TokenKind::Great {
                self.scan_jsx_token(true);
                let opening =
                    self.create_jsx_opening_ele(self.new_span(start), tag_name, ty_args, attrs);
                let mut children = self.parse_jsx_children(Some(opening.tag_name));
                let mut closing_ele = None;

                let late_child = children.last();
                let mut else_then = true;
                if let Some(end) = late_child
                    && let bolt_ts_ast::JsxChild::Elem(end_ele) = end
                    && !tag_names_are_eq(
                        &end_ele.opening_elem.tag_name,
                        &end_ele.closing_elem.tag_name,
                    )
                    && tag_names_are_eq(&opening.tag_name, &end_ele.closing_elem.tag_name)
                {
                    let span = Span::new(self.pos as u32, self.pos as u32, self.module_id);
                    let ident = self.create_ident_by_atom(keyword::IDENT_EMPTY, span);
                    let closing =
                        self.create_jsx_closing_ele(span, bolt_ts_ast::JsxTagName::Ident(ident));
                    let span = self.new_span(end.span().lo());
                    let last = self.create_jsx_ele(span, opening, children, closing);

                    let mut list = children[..children.len() - 1].to_vec();
                    list.push(bolt_ts_ast::JsxChild::Elem(last));
                    else_then = false;
                    children = self.alloc(list);
                    closing_ele = Some(end_ele.closing_elem);
                }
                if else_then {
                    let e = self.parse_jsx_closing_ele(opening, in_expr_context, opening_tag)?;
                    closing_ele = Some(e);
                } else {
                    unreachable!()
                }

                result = JsxEleOrSelfClosingEleOrFrag::Ele(self.create_jsx_ele(
                    self.new_span(start),
                    opening,
                    children,
                    closing_ele.unwrap(),
                ));
            } else {
                self.expect(TokenKind::Slash);
                if self.expect_with::<false>(TokenKind::Great, {
                    let f: Option<fn(&mut Self) -> crate::Diag> = None;
                    f
                }) {
                    if in_expr_context {
                        self.next_token();
                    } else {
                        self.scan_jsx_token(true);
                    }
                }
                result =
                    JsxEleOrSelfClosingEleOrFrag::SelfClosingEle(self.create_jsx_self_closing_ele(
                        self.new_span(start),
                        tag_name,
                        ty_args,
                        attrs,
                    ));
            }
        }

        if in_expr_context && !must_be_unary && self.token.kind == TokenKind::Less {
            let top_bad_pos = if let Some(pos) = top_invalid_node_position {
                pos
            } else {
                result.span().lo()
            };
            if let Ok(Some(invalid_ele)) = self.try_parse(|this| {
                if let Ok(result) = this.p().parse_jsx_ele_or_self_closing_ele_or_frag(
                    true,
                    Some(top_bad_pos),
                    None,
                    false,
                ) {
                    Ok(Some(result))
                } else {
                    Err(())
                }
            }) {
                todo!("error handle: {:#?}", invalid_ele);
            }
        }

        Ok(result)
    }

    fn parse_jsx_text(&mut self) -> &'cx bolt_ts_ast::JsxText {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let text = self.string_token();
        let contains_only_trivia_whitespace = self.token.kind == TokenKind::JSXTextAllWhiteSpaces;
        let node = self.create_jsx_text(text, self.token.span, contains_only_trivia_whitespace);
        self.scan_jsx_token(true);
        node
    }

    fn parse_jsx_child(
        &mut self,
        opening_tag_name: Option<bolt_ts_ast::JsxTagName<'cx>>,
        token: TokenKind,
    ) -> PResult<Option<bolt_ts_ast::JsxChild<'cx>>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        match token {
            TokenKind::EOF => {
                if let Some(opening_tag_name) = opening_tag_name {
                    todo!("error handle")
                } else {
                    todo!("error handle")
                }
                Ok(None)
            }
            TokenKind::LessSlash => {
                // TODO: conflict marker trivia
                Ok(None)
            }
            TokenKind::JSXText | TokenKind::JSXTextAllWhiteSpaces => {
                Ok(Some(bolt_ts_ast::JsxChild::Text(self.parse_jsx_text())))
            }
            TokenKind::LBrace => self
                .parse_jsx_expr(false)
                .map(|e| e.map(bolt_ts_ast::JsxChild::Expr)),
            TokenKind::Less => Ok(Some(
                match self.parse_jsx_ele_or_self_closing_ele_or_frag(
                    false,
                    None,
                    opening_tag_name,
                    false,
                )? {
                    JsxEleOrSelfClosingEleOrFrag::Ele(n) => bolt_ts_ast::JsxChild::Elem(n),
                    JsxEleOrSelfClosingEleOrFrag::SelfClosingEle(n) => {
                        bolt_ts_ast::JsxChild::SelfClosingEle(n)
                    }
                    JsxEleOrSelfClosingEleOrFrag::Frag(n) => bolt_ts_ast::JsxChild::Frag(n),
                },
            )),
            _ => unreachable!(),
        }
    }

    fn parse_jsx_children(
        &mut self,
        opening_tag_name: Option<bolt_ts_ast::JsxTagName<'cx>>,
    ) -> &'cx [bolt_ts_ast::JsxChild<'cx>] {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let mut list = Vec::with_capacity(16);
        // let start = self.token.start();
        // let save_parsing_context = self.parsecon

        loop {
            self.re_scan_jsx_token(true);
            let Ok(Some(child)) = self.parse_jsx_child(opening_tag_name, self.token.kind) else {
                break;
            };
            list.push(child);
            if let Some(opening_tag_name) = opening_tag_name
                && let bolt_ts_ast::JsxChild::Elem(ele) = child
                && !tag_names_are_eq(&ele.opening_elem.tag_name, &ele.opening_elem.tag_name)
                && tag_names_are_eq(&opening_tag_name, &ele.closing_elem.tag_name)
            {
                break;
            }
        }
        self.alloc(list)
    }

    fn parse_jsx_attrs(&mut self) -> bolt_ts_ast::JsxAttrs<'cx> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let attrs = self.parse_list(ParsingContext::JSX_ATTRIBUTES, Self::parse_jsx_attr);
        self.create_jsx_attrs(attrs)
    }

    fn parse_jsx_ele_name(&mut self) -> PResult<JsxTagName<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        let init_expr = self.parse_jsx_tag_name()?;

        use bolt_ts_ast::ExprKind;

        let kind = match init_expr {
            JsxTagName::Ident(n) => ExprKind::Ident(n),
            JsxTagName::This(n) => ExprKind::This(n),
            JsxTagName::Ns(n) => return Ok(JsxTagName::Ns(n)),
            JsxTagName::PropAccess(_) => unreachable!(),
        };
        let mut expr = self.alloc(bolt_ts_ast::Expr { kind });
        while self.parse_optional(TokenKind::Dot).is_some() {
            let name = self.parse_right_side_of_dot(true)?;
            let prop = self.create_prop_access_expr(start, expr, name);
            expr = self.alloc(bolt_ts_ast::Expr {
                kind: ExprKind::PropAccess(prop),
            });
        }
        match expr.kind {
            ExprKind::Ident(n) => Ok(JsxTagName::Ident(n)),
            ExprKind::This(n) => Ok(JsxTagName::This(n)),
            ExprKind::PropAccess(n) => Ok(JsxTagName::PropAccess(n)),
            _ => unreachable!(),
        }
    }

    fn parse_jsx_tag_name(&mut self) -> PResult<JsxTagName<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        self.scan_jsx_ident();
        let is_this = self.token.kind == TokenKind::This;
        let tag_name = self.parse_identifier_name_error_or_unicode_escape_sequence()?;
        Ok(if self.parse_optional(TokenKind::Colon).is_some() {
            self.scan_jsx_ident();
            let name = self.parse_identifier_name_error_or_unicode_escape_sequence()?;
            let span = self.new_span(start);
            JsxTagName::Ns(self.create_jsx_ns_name(tag_name, name, span))
        } else if is_this {
            let span = self.new_span(start);
            debug_assert!(span.lo() + keyword::KW_THIS_STR.len() as u32 == span.hi());
            let this = self.create_this_expr(span);
            JsxTagName::This(this)
        } else {
            JsxTagName::Ident(tag_name)
        })
    }

    fn parse_jsx_expr(
        &mut self,
        in_expr_context: bool,
    ) -> PResult<Option<&'cx bolt_ts_ast::JsxExpr<'cx>>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        if !self.expect(TokenKind::LBrace) {
            Ok(None)
        } else {
            let mut dotdotdot_token = None;
            let mut expr = None;
            if self.token.kind != TokenKind::RBrace {
                if !in_expr_context {
                    dotdotdot_token = self.parse_optional(TokenKind::DotDotDot).map(|t| t.span);
                }
                expr = Some(self.parse_expr()?);
            }

            if in_expr_context {
                self.expect(TokenKind::RBrace);
            } else if self.expect_with::<false>(TokenKind::RBrace, {
                let f: Option<fn(&mut Self) -> crate::Diag> = None;
                f
            }) {
                self.scan_jsx_token(true);
            }

            let span = self.new_span(start);
            Ok(Some(self.create_jsx_expr(dotdotdot_token, expr, span)))
        }
    }

    fn parse_jsx_attr(&mut self) -> PResult<bolt_ts_ast::JsxAttr<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        Ok(if self.token.kind == TokenKind::LBrace {
            bolt_ts_ast::JsxAttr::Spread(self.parse_jsx_spread_attr()?)
        } else {
            let start = self.token.start();
            let name = self.parse_jsx_attr_name()?;
            let init = self.parse_jsx_attr_value();
            let span = self.new_span(start);
            bolt_ts_ast::JsxAttr::Named(self.create_jsx_named_attr(name, init, span))
        })
    }

    fn parse_jsx_attr_value(&mut self) -> Option<bolt_ts_ast::JsxAttrValue<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        use bolt_ts_ast::JsxAttrValue::*;
        if self.token.kind == TokenKind::Eq {
            self.scan_jsx_attr_value();
            match self.token.kind {
                TokenKind::String => Some(StringLit(self.parse_string_lit())),
                TokenKind::LBrace => {
                    if let Ok(Some(e)) = self.parse_jsx_expr(true) {
                        Some(Expr(e))
                    } else {
                        None
                    }
                }
                TokenKind::Less => Some(
                    match self
                        .parse_jsx_ele_or_self_closing_ele_or_frag(true, None, None, false)
                        .ok()?
                    {
                        JsxEleOrSelfClosingEleOrFrag::Ele(n) => Ele(n),
                        JsxEleOrSelfClosingEleOrFrag::SelfClosingEle(n) => SelfClosingEle(n),
                        JsxEleOrSelfClosingEleOrFrag::Frag(n) => Frag(n),
                    },
                ),
                _ => {
                    self.push_error(Box::new(errors::OrJsxElementExpected {
                        span: self.token.span,
                    }));
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse_jsx_attr_name(&mut self) -> PResult<bolt_ts_ast::JsxAttrName<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        self.scan_jsx_ident();
        let name = self.parse_identifier_name_error_or_unicode_escape_sequence()?;
        Ok(if self.parse_optional(TokenKind::Colon).is_some() {
            self.scan_jsx_ident();
            let n = self.parse_identifier_name_error_or_unicode_escape_sequence()?;
            let span = self.new_span(start);
            bolt_ts_ast::JsxAttrName::Ns(self.create_jsx_ns_name(name, n, span))
        } else {
            bolt_ts_ast::JsxAttrName::Ident(name)
        })
    }

    fn parse_jsx_spread_attr(&mut self) -> PResult<&'cx bolt_ts_ast::JsxSpreadAttr<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        self.expect(TokenKind::LBrace);
        self.expect(TokenKind::DotDotDot);
        let expr = self.parse_expr()?;
        self.expect(TokenKind::RBrace);
        Ok(self.create_jsx_spread_attr(expr, self.new_span(start)))
    }

    fn parse_jsx_closing_ele(
        &mut self,
        opening: &'cx bolt_ts_ast::JsxOpeningElem<'cx>,
        in_expr_context: bool,
        opening_tag: Option<bolt_ts_ast::JsxTagName<'cx>>,
    ) -> PResult<&'cx bolt_ts_ast::JsxClosingElem<'cx>> {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        self.expect(TokenKind::LessSlash);
        let tag_name = self.parse_jsx_ele_name()?;
        let wf = tag_names_are_eq(&opening.tag_name, &tag_name);
        if self.expect_with::<false>(TokenKind::Great, {
            let f: Option<fn(&mut Self) -> crate::Diag> = None;
            f
        }) {
            if in_expr_context || !wf {
                self.next_token();
            } else {
                self.scan_jsx_token(true);
            }
        }
        if !wf {
            if opening_tag.is_some_and(|t| tag_names_are_eq(&tag_name, &t)) {
                todo!("error handle");
            } else {
                self.push_error(Box::new(
                    errors::ExpectedCorrespondingJsxClosingTagForOpeningTagName {
                        span: tag_name.span(),
                        opening_tag_name: {
                            let span = opening.tag_name.span();
                            let s = &self.input[span.lo() as usize..span.hi() as usize];
                            unsafe { String::from_utf8_unchecked(s.to_vec()) }
                        },
                    },
                ));
            }
        }

        Ok(self.create_jsx_closing_ele(self.new_span(start), tag_name))
    }

    fn parse_jsx_closing_frag(
        &mut self,
        in_expr_context: bool,
    ) -> &'cx bolt_ts_ast::JsxClosingFrag {
        debug_assert!(self.variant == LanguageVariant::Jsx);
        let start = self.token.start();
        self.expect(TokenKind::LessSlash);

        if self.expect_with::<false>(
            TokenKind::Great,
            Some(|this: &mut ParserState<'cx, 'p>| {
                Box::new(errors::ExpectedCorrespondingClosingTagForJsxFragment {
                    span: this.token.span,
                }) as _
            }),
        ) {
            if in_expr_context {
                self.next_token();
            } else {
                self.scan_jsx_token(true);
            }
        }

        self.create_jsx_jsx_closing_fragment(self.new_span(start))
    }
}

fn tag_names_are_eq(a: &bolt_ts_ast::JsxTagName<'_>, b: &bolt_ts_ast::JsxTagName<'_>) -> bool {
    use bolt_ts_ast::JsxTagName::*;

    fn expr_eq(a: &bolt_ts_ast::Expr, b: &bolt_ts_ast::Expr) -> bool {
        use bolt_ts_ast::ExprKind::*;
        debug_assert!(matches!(a.kind, Ident(_) | This(_) | PropAccess(_)));
        debug_assert!(matches!(b.kind, Ident(_) | This(_) | PropAccess(_)));
        match (a.kind, b.kind) {
            (Ident(a), Ident(b)) => a.name == b.name,
            (This(_), This(_)) => true,
            (PropAccess(a), PropAccess(b)) => a.name.name == b.name.name && expr_eq(a.expr, b.expr),
            _ => false,
        }
    }

    match (a, b) {
        (Ident(a), Ident(b)) => a.name == b.name,
        (This(_), This(_)) => true,
        (Ns(a), Ns(b)) => a.ns.name == b.ns.name && a.name.name == b.name.name,
        (PropAccess(a), PropAccess(b)) => a.name.name == b.name.name && expr_eq(a.expr, b.expr),
        _ => false,
    }
}
