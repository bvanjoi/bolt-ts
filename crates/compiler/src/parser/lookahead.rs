use super::token::TokenKind;
use super::{PResult, ParserState, Tristate};

impl ParserState<'_, '_> {
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
        if matches!(self.token.kind, TokenKind::Plus | TokenKind::Minus) {
            self.next_token();
            return Ok(self.token.kind == TokenKind::Readonly);
        } else if self.token.kind == TokenKind::Readonly {
            self.next_token();
        }
        Ok(
            self.token.kind == TokenKind::LBracket && self.next_token_is_ident()? && {
                self.next_token();
                self.token.kind == TokenKind::In
            },
        )
    }

    pub(super) fn next_token_is_from_keyword_or_eq_token(&mut self) -> bool {
        self.next_token();
        matches!(self.token.kind, TokenKind::From | TokenKind::Eq)
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

    pub(super) fn is_start_of_ty_of_import_ty(&mut self) -> bool {
        self.next_token();
        self.token.kind == TokenKind::Import
    }

    fn is_decl(&mut self) -> bool {
        use TokenKind::*;
        loop {
            match self.token.kind {
                Var | Let | Const | Function | Class | Enum => return true,
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
                    let mut current = self.token.kind;
                    if current == Type {
                        current = self.lookahead(|this| {
                            this.next_token();
                            this.token.kind
                        });
                    }
                    if matches!(current, Eq | Asterisk | LBrace | Default | As | At) {
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

    fn _is_paren_arrow_fn_expr(&mut self) -> Tristate {
        use TokenKind::*;
        if self.token.kind == TokenKind::Async {
            self.next_token();
            if self.has_preceding_line_break() || !matches!(self.token.kind, LParen | Less) {
                return Tristate::False;
            }
        }

        let first = self.token.kind;
        self.next_token();
        let second = self.token.kind;

        if first == LParen {
            if second == RParen {
                self.next_token();
                if matches!(self.token.kind, EqGreat | Colon | RBrace) {
                    Tristate::True
                } else {
                    Tristate::False
                }
            } else if second == LBracket || second == LBrace {
                Tristate::Unknown
            } else if second == DotDotDot {
                Tristate::True
            } else if second != Async
                && second.is_modifier_kind()
                && self
                    .lookahead(Self::next_token_is_ident)
                    .unwrap_or_default()
            {
                self.next_token();
                if self.token.kind == As {
                    Tristate::False
                } else {
                    Tristate::True
                }
            } else if !self.is_ident() && second != This {
                Tristate::False
            } else {
                self.next_token();
                match self.token.kind {
                    Colon => Tristate::True,
                    Question => {
                        self.next_token();
                        if matches!(self.token.kind, Colon | Comma | Eq | RParen) {
                            Tristate::True
                        } else {
                            Tristate::False
                        }
                    }
                    Comma | Eq | RParen => Tristate::Unknown,
                    _ => Tristate::False,
                }
            }
        } else {
            assert_eq!(first, Less);
            if !self.is_ident() && self.token.kind != Const {
                Tristate::False
            } else {
                Tristate::Unknown
            }
        }
    }

    pub(super) fn is_paren_arrow_fn_expr(&mut self) -> Tristate {
        let t = self.token.kind;

        if matches!(t, TokenKind::LParen | TokenKind::Less | TokenKind::Async) {
            return self.lookahead(Self::_is_paren_arrow_fn_expr);
        }

        if t == TokenKind::EqGreat {
            // ERROR RECOVERY TWEAK:
            // If we see a standalone => try to parse it as an arrow function expression as that's
            // likely what the user intended to write.
            return Tristate::True;
        }

        Tristate::False
    }

    pub(super) fn next_token_is_class_kw_on_same_line(&mut self) -> bool {
        self.next_token();
        self.token.kind == TokenKind::Class && !self.has_preceding_line_break()
    }

    pub(super) fn next_token_is_function_kw_on_same_line(&mut self) -> bool {
        self.next_token();
        self.token.kind == TokenKind::Function && !self.has_preceding_line_break()
    }
}
