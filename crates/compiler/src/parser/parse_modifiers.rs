use super::{PResult, ParserState};
use bolt_ts_ast as ast;
use bolt_ts_ast::TokenKind;

impl<'cx> ParserState<'cx, '_> {
    fn can_follow_modifier(&self) -> bool {
        let t = self.token.kind;
        use bolt_ts_ast::TokenKind::*;
        matches!(t, LBracket | LBrace | Asterisk | DotDotDot) || t.is_lit_prop_name()
    }

    fn next_token_is_on_same_line_and_can_follow_modifier(&mut self) -> bool {
        self.next_token();
        if self.has_preceding_line_break() {
            false
        } else {
            self.can_follow_modifier()
        }
    }

    fn can_follow_export_modifier(&self) -> bool {
        let t = self.token.kind;
        use bolt_ts_ast::TokenKind::*;
        (t == TokenKind::At) || (!matches!(t, Asterisk | As | LBrace) && self.can_follow_modifier())
    }

    fn next_token_can_follow_default_keyword(&mut self) -> bool {
        self.next_token();
        let t = self.token.kind;
        use bolt_ts_ast::TokenKind::*;
        matches!(t, Class | Function | Interface | At)
            || (t == Abstract && self.lookahead(Self::next_token_is_class_kw_on_same_line))
            || (t == Async && self.lookahead(Self::next_token_is_function_kw_on_same_line))
    }

    fn next_token_can_follow_export_modifier(&mut self) -> bool {
        self.next_token();
        self.can_follow_export_modifier()
    }

    pub(super) fn next_token_can_follow_modifier(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        match self.token.kind {
            Const => {
                self.next_token();
                self.token.kind == Enum
            }
            Export => {
                self.next_token();
                let t = self.token.kind;
                if t == Default {
                    self.lookahead(Self::next_token_can_follow_default_keyword)
                } else if t == Type {
                    self.lookahead(Self::next_token_can_follow_export_modifier)
                } else {
                    self.can_follow_export_modifier()
                }
            }
            Default => self.next_token_can_follow_default_keyword(),
            _ => self.next_token_is_on_same_line_and_can_follow_modifier(),
        }
    }

    pub(super) fn next_token_is_ident_or_keyword_or_open_bracket_or_template(
        &mut self,
    ) -> PResult<bool> {
        self.next_token();
        Ok(self.token.kind.is_ident_or_keyword()
            || self.token.kind == TokenKind::LBracket
            || self.is_template_start_of_tagged_template())
    }

    fn parse_any_contextual_modifier(&mut self) -> bool {
        self.token.kind.is_modifier_kind() && self.try_parse(Self::next_token_can_follow_modifier)
    }

    pub(super) fn parse_modifier(
        &mut self,
        has_seen_static_modifier: bool,
        permit_const_as_modifier: Option<bool>,
    ) -> PResult<Option<&'cx ast::Modifier>> {
        let span = self.token.span;
        let t = self.token.kind;
        if t == TokenKind::Const && permit_const_as_modifier.unwrap_or_default() {
            if self.try_parse(Self::next_token_is_on_same_line_and_can_follow_modifier) {
                return Ok(None);
            }
        } else if has_seen_static_modifier && t == TokenKind::Static {
            return Ok(None);
        } else if !self.parse_any_contextual_modifier() {
            return Ok(None);
        }

        let id = self.next_node_id();
        let kind = t.try_into().unwrap();
        let m = self.alloc(ast::Modifier { id, span, kind });
        self.nodes.insert(id, ast::Node::Modifier(m));
        Ok(Some(m))
    }
}
