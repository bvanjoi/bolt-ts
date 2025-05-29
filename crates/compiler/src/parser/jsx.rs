use bolt_ts_ast::{TokenKind, keyword};

use super::{PResult, ParserState, list_ctx};

impl<'cx, 'p> ParserState<'cx, 'p> {
    fn parse_jsx_ele_or_self_closing_ele_or_frag(&mut self, is_expr_context: bool) {}

    fn parse_jsx_text(&mut self) -> &'cx bolt_ts_ast::JsxText {
        let text = self.string_token();
        let contains_only_trivia_whitespace = self.token.kind == TokenKind::JSXTextAllWhiteSpaces;
        let node = self.create_jsx_text(text, self.token.span, contains_only_trivia_whitespace);

        self.scan_jsx_token(true);

        node
    }

    fn parse_jsx_attrs(&mut self) {
        let attrs = self.parse_list(list_ctx::JsxAttrs, Self::parse_jsx_attr);
        self.create_jsx_attrs(attrs);
    }

    fn parse_jsx_ele_name() {}

    fn parse_jsx_tag_name(&mut self) -> PResult<bolt_ts_ast::JsxTagName<'cx>> {
        let start = self.token.start();
        self.scan_jsx_ident();
        let is_this = self.token.kind == TokenKind::This;
        let tag_name = self.parse_identifier_name_error_or_unicode_escape_sequence()?;
        Ok(if self.parse_optional(TokenKind::Colon).is_some() {
            self.scan_jsx_ident();
            let name = self.parse_identifier_name_error_or_unicode_escape_sequence()?;
            let span = self.new_span(start);
            bolt_ts_ast::JsxTagName::Ns(self.create_jsx_ns_name(tag_name, name, span))
        } else if is_this {
            let span = self.new_span(start);
            debug_assert!(span.lo + keyword::KW_THIS_STR.len() as u32 == span.hi);
            let this = self.create_this_expr(span);
            bolt_ts_ast::JsxTagName::This(this)
        } else {
            bolt_ts_ast::JsxTagName::Ident(tag_name)
        })
    }

    fn parse_jsx_expr(
        &mut self,
        in_expr_context: bool,
    ) -> PResult<Option<&'cx bolt_ts_ast::JsxExpr<'cx>>> {
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
        Ok(if self.token.kind == TokenKind::LBrace {
            bolt_ts_ast::JsxAttr::Spread(self.parse_jsx_spread_attr()?)
        } else {
            let start = self.token.start();
            let name = self.parse_jsx_attr_name()?;
            let init = self.parse_jsx_attr_value()?;
            let span = self.new_span(start);
            bolt_ts_ast::JsxAttr::Named(self.create_jsx_named_attr(name, init, span))
        })
    }

    fn parse_jsx_attr_value(&mut self) -> PResult<Option<bolt_ts_ast::JsxAttrValue<'cx>>> {
        use bolt_ts_ast::JsxAttrValue::*;
        if self.token.kind == TokenKind::Eq {
            self.scan_jsx_attr_value();
            match self.token.kind {
                TokenKind::String => Ok(Some(StringLit(self.parse_string_lit()))),
                TokenKind::LBrace => {
                    if let Some(e) = self.parse_jsx_expr(true)? {
                        Ok(Some(JsxExpr(e)))
                    } else {
                        Ok(None)
                    }
                }
                TokenKind::Less => {
                    todo!()
                    // self.parse_jsx_ele_or_self_closing_ele_or_frag(true);
                }
                _ => todo!("error handle"),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_jsx_attr_name(&mut self) -> PResult<bolt_ts_ast::JsxAttrName<'cx>> {
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
        let start = self.token.start();
        self.expect(TokenKind::LBrace);
        self.expect(TokenKind::DotDotDot);
        let expr = self.parse_expr()?;
        self.expect(TokenKind::RBrace);
        Ok(self.create_jsx_spread_attr(expr, self.new_span(start)))
    }

    fn parse_jsx_closing_ele(&mut self) {}

    fn parse_jsx_closing_frag(
        &mut self,
        in_expr_context: bool,
    ) -> &'cx bolt_ts_ast::JsxClosingFrag {
        let start = self.token.start();
        self.expect(TokenKind::LessSlash);

        // TODO: use `expect_with`
        if self.expect(TokenKind::Great) {
            if in_expr_context {
                self.next_token();
            } else {
                self.scan_jsx_token(true);
            }
        }

        self.create_jsx_jsx_closing_fragment(self.new_span(start))
    }
}
