use super::token::TokenKind;
use super::{PResult, ParserState};

impl<'p, 't> ParserState<'p, 't> {
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

    fn next_token_is_identifier_on_same_line(&mut self) -> bool {
        self.next_token();
        !self.has_preceding_line_break() && self.is_ident()
    }

    fn next_token_is_identifier_or_string_literal_on_same_line(&mut self) -> bool {
        self.next_token();
        !self.has_preceding_line_break()
            && (self.is_ident() || self.token.kind == TokenKind::String)
    }

    fn is_decl(&mut self) -> bool {
        use TokenKind::*;
        loop {
            match self.token.kind {
                Var | Let | Const | Function | Class => return true,
                Abstract | Declare | Public | Private => {
                    // let prev = self.token.kind;
                    self.next_token();
                    continue;
                }
                Interface | Type => return self.next_token_is_identifier_on_same_line(),
                Module | Namespace => {
                    return self.next_token_is_identifier_or_string_literal_on_same_line()
                }
                Import => {
                    self.next_token();
                    return matches!(self.token.kind, String | Asterisk | LBrace)
                        || self.token.kind.is_ident_or_keyword();
                }
                Export => {
                    self.next_token();
                    if self.token.kind == Type {
                        self.next_token();
                    }
                    if matches!(self.token.kind, Eq | Asterisk | LBrace | Default | As | At) {
                        return true;
                    }
                }
                _ => unreachable!("{:#?}", self.token.kind),
            }
        }
    }

    pub(super) fn is_start_of_decl(&mut self) -> bool {
        self.lookahead(Self::is_decl)
    }
}
