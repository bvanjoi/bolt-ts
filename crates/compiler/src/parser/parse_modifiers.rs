use crate::parser::lookahead::Lookahead;

use super::{PResult, ParserState};
use bolt_ts_ast as ast;
use bolt_ts_ast::TokenKind;

impl<'cx> ParserState<'cx, '_> {
    pub(super) fn can_follow_modifier(&self) -> bool {
        let t = self.token.kind;
        use bolt_ts_ast::TokenKind::*;
        matches!(t, LBracket | LBrace | Asterisk | DotDotDot) || t.is_lit_prop_name()
    }

    pub(super) fn can_follow_export_modifier(&self) -> bool {
        let t = self.token.kind;
        use bolt_ts_ast::TokenKind::*;
        (t == TokenKind::At) || (!matches!(t, Asterisk | As | LBrace) && self.can_follow_modifier())
    }

    fn parse_any_contextual_modifier(&mut self) -> bool {
        self.token.kind.is_modifier_kind()
            && self.try_parse(Lookahead::next_token_can_follow_modifier)
    }

    pub(super) fn parse_modifier(
        &mut self,
        has_seen_static_modifier: bool,
        permit_const_as_modifier: Option<bool>,
    ) -> PResult<Option<&'cx ast::Modifier>> {
        let span = self.token.span;
        let t = self.token.kind;
        if t == TokenKind::Const && permit_const_as_modifier.unwrap_or_default() {
            if self.try_parse(Lookahead::next_token_is_on_same_line_and_can_follow_modifier) {
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
