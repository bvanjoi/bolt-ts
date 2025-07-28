use crate::state::LanguageVariant;

use super::{PResult, ParserState, Tristate, utils::ParseSuccess};
use bolt_ts_ast::{TokenKind, keyword};

pub(super) struct Lookahead<'a, 'cx, 'p> {
    p: &'a mut ParserState<'cx, 'p>,
}

impl<'a, 'cx, 'p> Lookahead<'a, 'cx, 'p> {
    #[inline(always)]
    pub(super) fn p(&mut self) -> &mut ParserState<'cx, 'p> {
        self.p
    }

    pub(super) fn next_token_is_ident_or_keyword_or_open_bracket_or_template(
        &mut self,
    ) -> PResult<bool> {
        self.p.next_token();
        Ok(self.p.token.kind.is_ident_or_keyword()
            || self.p.token.kind == TokenKind::LBracket
            || self.p.is_template_start_of_tagged_template())
    }

    pub(super) fn next_token_is_ident_or_keyword_or_great(&mut self) -> PResult<bool> {
        self.p.next_token();
        Ok(self.p.token.kind.is_ident_or_keyword() || self.p.token.kind == TokenKind::Great)
    }

    fn is_next_token_colon_or_question_colon(&mut self) -> bool {
        self.p.next_token();
        self.p.token.kind == TokenKind::Colon
            || (self.p.token.kind == TokenKind::Question && {
                self.p.next_token();
                self.p.token.kind == TokenKind::Colon
            })
    }

    pub(super) fn is_tuple_ele_name(&mut self) -> bool {
        if self.p.token.kind == TokenKind::DotDotDot {
            self.p.next_token();
        }
        self.p.token.kind.is_ident_or_keyword() && self.is_next_token_colon_or_question_colon()
    }

    fn in_try_context<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
        need_revert: impl FnOnce(&T) -> bool,
    ) -> T {
        let old_pos = self.p.pos;
        let old_full_start_pos = self.p.full_start_pos;
        let old_token = self.p.token;
        let old_token_value = self.p.token_value;
        let old_line = self.p.line;
        let old_token_flags = self.p.token_flags;
        let old_line_start = self.p.line_start;
        let old_line_map_len = self.p.line_map.len();
        let old_parse_diag_len = self.p.diags.len();
        let old_current_node_id = self.p.current_node_id();

        let r = f(self);

        if need_revert(&r) {
            self.p.line_map.truncate(old_line_map_len);
            self.p.line_start = old_line_start;
            self.p.token_flags = old_token_flags;
            self.p.line = old_line;
            self.p.token_value = old_token_value;
            self.p.token = old_token;
            self.p.full_start_pos = old_full_start_pos;
            self.p.pos = old_pos;
            // TODO: speculate_kind != SpeculationKind::Repair
            self.p.diags.truncate(old_parse_diag_len);
            self.p.reset_node_id(old_current_node_id);
        }

        r
    }

    pub(super) fn lookahead<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.in_try_context(f, |_| true)
    }

    pub(super) fn is_start_of_mapped_ty(&mut self) -> PResult<bool> {
        self.p.next_token();
        if matches!(self.p.token.kind, TokenKind::Plus | TokenKind::Minus) {
            self.p.next_token();
            return Ok(self.p.token.kind == TokenKind::Readonly);
        } else if self.p.token.kind == TokenKind::Readonly {
            self.p.next_token();
        }
        Ok(
            self.p.token.kind == TokenKind::LBracket && self.next_token_is_ident()? && {
                self.p.next_token();
                self.p.token.kind == TokenKind::In
            },
        )
    }

    pub(super) fn next_token_is_from_keyword_or_eq_token(&mut self) -> bool {
        self.p.next_token();
        matches!(self.p.token.kind, TokenKind::From | TokenKind::Eq)
    }

    pub(super) fn next_token_is_numeric_or_big_int_literal(&mut self) -> bool {
        self.p.next_token();
        matches!(self.p.token.kind, TokenKind::Number | TokenKind::BigInt)
    }

    fn next_token_is_identifier_on_same_line(&mut self) -> bool {
        self.p.next_token();
        !self.p.has_preceding_line_break() && self.p.is_ident()
    }

    fn next_token_is_identifier_or_string_literal_on_same_line(&mut self) -> bool {
        self.p.next_token();
        !self.p.has_preceding_line_break()
            && (self.p.is_ident() || self.p.token.kind == TokenKind::String)
    }

    fn is_decl(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        loop {
            match self.p.token.kind {
                Var | Let | Const | Function | Class | Enum => return true,
                Abstract | Async | Declare | Public | Private | Protected /* TODO: Accessor */=> {
                    let prev = self.p.token.kind;
                    self.p.next_token();
                    if self.p.has_preceding_line_break() {
                        return false;
                    } else if prev == Declare && self.p.token.kind == Type {
                        return true;
                    } else {
                        continue;
                    }
                }
                Interface | Type => return self.next_token_is_identifier_on_same_line(),
                Module | Namespace => {
                    return self.next_token_is_identifier_or_string_literal_on_same_line();
                }
                Import => {
                    self.p.next_token();
                    return matches!(self.p.token.kind, String | Asterisk | LBrace)
                        || self.p.token.kind.is_ident_or_keyword();
                }
                Export => {
                    self.p.next_token();
                    let mut current = self.p.token.kind;
                    if current == Type {
                        current = self.lookahead(|this| {
                            this.p.next_token();
                            this.p.token.kind
                        });
                    }
                    if matches!(current, Eq | Asterisk | LBrace | Default | As | At) {
                        return true;
                    }
                }
                Ident if self.p.ident_token() == keyword::IDENT_GLOBAL => {
                    self.p.next_token();
                    return matches!(self.p.token.kind, LBrace | Ident | Export);
                }
                _ => return false,
            }
        }
    }

    pub(super) fn is_start_of_ty_of_import_ty(&mut self) -> bool {
        self.p.next_token();
        self.p.token.kind == TokenKind::Import
    }

    pub(super) fn is_invalid_heritage_clause_object(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        assert!(self.p.token.kind == LBrace);
        self.p.next_token();
        if self.p.token.kind == RBrace {
            self.p.next_token();
            return matches!(self.p.token.kind, Comma | LBrace | Extends | Implements);
        }
        true
    }

    pub(super) fn next_token_is_start_of_expr(&mut self) -> bool {
        self.p.next_token();
        self.p.is_start_of_expr()
    }

    pub(super) fn next_token_is_class_kw_on_same_line(&mut self) -> bool {
        self.p.next_token();
        self.p.token.kind == TokenKind::Class && !self.p.has_preceding_line_break()
    }

    pub(super) fn next_token_is_function_kw_on_same_line(&mut self) -> bool {
        self.p.next_token();
        self.p.token.kind == TokenKind::Function && !self.p.has_preceding_line_break()
    }

    pub(super) fn next_token_can_follow_default_keyword(&mut self) -> bool {
        self.p.next_token();
        let t = self.p.token.kind;
        use bolt_ts_ast::TokenKind::*;
        matches!(t, Class | Function | Interface | At)
            || (t == Abstract && self.lookahead(Lookahead::next_token_is_class_kw_on_same_line))
            || (t == Async && self.lookahead(Lookahead::next_token_is_function_kw_on_same_line))
    }

    pub(super) fn next_token_is_ident(&mut self) -> PResult<bool> {
        self.p.next_token();
        Ok(self.p.is_ident())
    }

    fn _is_paren_arrow_fn_expr(&mut self) -> Tristate {
        use bolt_ts_ast::TokenKind::*;
        if self.p.token.kind == TokenKind::Async {
            self.p.next_token();
            if self.p.has_preceding_line_break() || !matches!(self.p.token.kind, LParen | Less) {
                return Tristate::False;
            }
        }

        let first = self.p.token.kind;
        self.p.next_token();
        let second = self.p.token.kind;

        if first == LParen {
            if second == RParen {
                self.p.next_token();
                if matches!(self.p.token.kind, EqGreat | Colon | RBrace) {
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
                    .lookahead(Lookahead::next_token_is_ident)
                    .unwrap_or_default()
            {
                self.p.next_token();
                if self.p.token.kind == As {
                    Tristate::False
                } else {
                    Tristate::True
                }
            } else if !self.p.is_ident() && second != This {
                Tristate::False
            } else {
                self.p.next_token();
                match self.p.token.kind {
                    Colon => Tristate::True,
                    Question => {
                        self.p.next_token();
                        if matches!(self.p.token.kind, Colon | Comma | Eq | RParen) {
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
            if !self.p.is_ident() && self.p.token.kind != Const {
                Tristate::False
            } else if self.p.variant == LanguageVariant::Jsx {
                let is_arrow_fn_in_jsx = self.lookahead(|this| {
                    this.p.parse_optional(Const);
                    this.p.next_token();
                    match this.p.token.kind {
                        Extends => {
                            this.p.next_token();
                            matches!(this.p.token.kind, Eq | Great | Slash)
                        }
                        Comma | Eq => true,
                        _ => false,
                    }
                });

                if is_arrow_fn_in_jsx {
                    Tristate::True
                } else {
                    Tristate::False
                }
            } else {
                Tristate::Unknown
            }
        }
    }

    fn next_token_can_follow_export_modifier(&mut self) -> bool {
        self.p.next_token();
        self.p.can_follow_export_modifier()
    }

    pub(super) fn next_token_can_follow_modifier(&mut self) -> bool {
        use bolt_ts_ast::TokenKind::*;
        match self.p.token.kind {
            Const => {
                self.p.next_token();
                self.p.token.kind == Enum
            }
            Export => {
                self.p.next_token();
                let t = self.p.token.kind;
                if t == Default {
                    self.lookahead(Lookahead::next_token_can_follow_default_keyword)
                } else if t == Type {
                    self.lookahead(Lookahead::next_token_can_follow_export_modifier)
                } else {
                    self.p.can_follow_export_modifier()
                }
            }
            Default => self.next_token_can_follow_default_keyword(),
            _ => self.next_token_is_on_same_line_and_can_follow_modifier(),
        }
    }

    pub(super) fn next_token_is_on_same_line_and_can_follow_modifier(&mut self) -> bool {
        self.p.next_token();
        if self.p.has_preceding_line_break() {
            false
        } else {
            self.p.can_follow_modifier()
        }
    }

    pub(super) fn next_token_is_lparen_or_less_or_dot(&mut self) -> bool {
        self.p.next_token();
        use bolt_ts_ast::TokenKind::*;
        matches!(self.p.token.kind, LParen | Less | Dot)
    }

    pub(super) fn next_token_is_ident_or_keyword(&mut self) -> PResult<bool> {
        self.p.next_token();
        Ok(self.p.token.kind.is_ident_or_keyword())
    }

    pub(super) fn next_token_is_ident_or_keyword_on_same_line(&mut self) -> PResult<bool> {
        self.p.next_token();
        Ok(self.p.token.kind.is_ident_or_keyword() && !self.p.has_preceding_line_break())
    }
}

impl<'a, 'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn try_parse<T: ParseSuccess>(
        &'a mut self,
        f: impl FnOnce(&mut Lookahead<'a, 'cx, 'p>) -> T,
    ) -> T {
        let mut l = Lookahead { p: self };
        l.in_try_context(f, |r| !r.is_success())
    }

    pub(super) fn lookahead<T>(
        &'a mut self,
        f: impl FnOnce(&mut Lookahead<'a, 'cx, 'p>) -> T,
    ) -> T {
        let mut l = Lookahead { p: self };
        l.lookahead(f)
    }

    pub(super) fn is_start_of_decl(&mut self) -> bool {
        self.lookahead(Lookahead::is_decl)
    }

    pub(super) fn is_paren_arrow_fn_expr(&mut self) -> Tristate {
        let t = self.token.kind;

        if matches!(t, TokenKind::LParen | TokenKind::Less | TokenKind::Async) {
            return self.lookahead(Lookahead::_is_paren_arrow_fn_expr);
        }

        if t == TokenKind::EqGreat {
            // ERROR RECOVERY TWEAK:
            // If we see a standalone => try to parse it as an arrow function expression as that's
            // likely what the user intended to write.
            return Tristate::True;
        }

        Tristate::False
    }
}
