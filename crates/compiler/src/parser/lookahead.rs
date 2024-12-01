use super::token::TokenKind;
use super::{PResult, ParserState};

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn is_tuple_ele_name(&mut self) -> bool {
        if self.token.kind == TokenKind::DotDotDot {
            self.next_token();
        }
        self.token.kind.is_ident_or_keyword() && self.is_next_token_colon_or_question_colon()
    }

    fn is_next_token_colon_or_question_colon(&mut self) -> bool {
        self.next_token();
        self.token.kind == TokenKind::Colon
            || (self.token.kind == TokenKind::Question && {
                self.next_token();
                self.token.kind == TokenKind::Colon
            })
    }

    pub(super) fn is_start_of_mapped_ty(&mut self) -> PResult<bool> {
        self.next_token();
        if self.token.kind == TokenKind::Plus || self.token.kind == TokenKind::Minus {
            self.next_token();
            // return self.token.kind == TokenKind::Readonly;
            todo!()
        }

        Ok(self.token.kind == TokenKind::LBracket
            && self.next_token_is_ident().unwrap_or_default()
            && {
                self.next_token();
                self.token.kind == TokenKind::In
            })
    }

    pub(super) fn next_token_is_numeric_or_big_int_literal(&mut self) -> bool {
        self.next_token();
        // TODO: big int lit
        matches!(self.token.kind, TokenKind::Number)
    }
}
