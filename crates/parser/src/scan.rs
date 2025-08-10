use std::{borrow::Cow, str};

use bolt_ts_span::Span;

use super::{CommentDirectiveKind, ParserState, TokenValue, errors, unicode};
use bolt_ts_ast::{RegularExpressionFlags, Token, TokenFlags, TokenKind, atom_to_token, keyword};

use crate::{CommentDirective, scan_integer::parse_integer};

#[inline(always)]
fn is_ascii_letter(ch: u8) -> bool {
    ch.is_ascii_alphabetic()
}

#[inline(always)]
fn is_word_character(ch: u8) -> bool {
    is_ascii_letter(ch) || ch.is_ascii_digit() || ch == b'_'
}

#[inline(always)]
fn is_ascii_identifier_start(ch: u8) -> bool {
    ch == b'$' || ch == b'_' || is_ascii_letter(ch)
}

#[inline(always)]
fn is_non_ascii_identifier_start(ch: u32, is_es5_target: bool) -> bool {
    debug_assert!(ch > 255);
    if is_es5_target {
        unicode::is_unicode_es5_identifier_start(ch)
    } else {
        unicode::is_unicode_esnext_identifier_start(ch)
    }
}

#[inline(always)]
fn is_identifier_start(ch: u32, is_es5_target: bool) -> bool {
    if ch <= 255 {
        is_ascii_identifier_start(ch as u8)
    } else {
        is_non_ascii_identifier_start(ch, is_es5_target)
    }
}

#[inline(always)]
fn is_ascii_identifier_part(ch: u8) -> bool {
    is_word_character(ch) || ch == b'$'
}

#[inline(always)]
fn is_identifier_part(ch: u32, is_es5_target: bool) -> bool {
    if ch <= 255 {
        is_ascii_identifier_part(ch as u8)
    } else if is_es5_target {
        unicode::is_unicode_es5_identifier_part(ch)
    } else {
        unicode::is_unicode_esnext_identifier_part(ch)
    }
}

#[inline(always)]
pub(super) fn is_line_break(ch: u8) -> bool {
    ch == b'\n' || ch == b'\r'
}

#[inline(always)]
fn is_octal_digit(ch: u8) -> bool {
    (b'0'..=b'7').contains(&ch)
}

const UTF8_CHAR_LEN_MAX: u32 = 6;

bitflags::bitflags! {
    pub struct EscapeSequenceScanningFlags: u32 {
        const STRING                  = 1 << 0;
        const REPORT_ERRORS           = 1 << 1;
        const REGULAR_EXPRESSION      = 1 << 2;
        const ANNEX_B                 = 1 << 3;
        const ANY_UNICODE_MODE        = 1 << 4;
        const ATOM_ESCAPE             = 1 << 5;

        const REPORT_INVALID_ESCAPE_ERRORS  = Self::REGULAR_EXPRESSION.bits() | Self::REPORT_ERRORS.bits();
        const ALLOW_EXTENDED_UNICODE_ESCAPE = Self::STRING.bits() | Self::ANY_UNICODE_MODE.bits();
    }
}

impl ParserState<'_, '_> {
    pub(super) fn ch(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    pub(super) fn ch_unchecked(&self) -> u8 {
        debug_assert!(self.pos < self.end());
        unsafe { *self.input.get_unchecked(self.pos) }
    }

    fn next_ch(&self) -> Option<u8> {
        self.input.get(self.pos + 1).copied()
    }

    fn next_ch_unchecked(&self) -> u8 {
        debug_assert!(self.pos + 1 < self.end());
        unsafe { *self.input.get_unchecked(self.pos + 1) }
    }

    fn next_next_ch(&self) -> Option<u8> {
        self.input.get(self.pos + 2).copied()
    }

    #[inline(always)]
    #[track_caller]
    pub(super) fn new_span(&self, lo: u32) -> Span {
        Span::new(lo, self.full_start_pos as u32, self.module_id)
    }

    fn scan_binary_or_octal_digits(&mut self, base: u8) -> Vec<u8> {
        assert!(base == 2 || base == 8);
        let mut sep_allowed = false;
        let mut is_prev_token_sep = false;
        let mut v = Vec::with_capacity(16);
        loop {
            let ch = self.ch_unchecked();
            if ch == b'_' {
                self.token_flags |= TokenFlags::CONTAINS_SEPARATOR;
                if sep_allowed {
                    sep_allowed = false;
                    is_prev_token_sep = true;
                } else if is_prev_token_sep {
                    todo!("error")
                } else {
                    todo!("error");
                }
                self.pos += 1;
                continue;
            }
            sep_allowed = true;
            if !ch.is_ascii_digit() || ch - b'0' >= base {
                break;
            }
            v.push(self.input[self.pos]);
            self.pos += 1;
            is_prev_token_sep = false;
        }
        if self.input[self.pos - 1] == b'_' {
            todo!("error");
        }
        v
    }

    fn scan_number_fragment(&mut self) {
        let mut allow_separator = false;
        let mut is_previous_token_separator = false;
        while let Some(ch) = self.ch() {
            if ch == b'_' {
                self.token_flags |= TokenFlags::CONTAINS_SEPARATOR;
                if allow_separator {
                    allow_separator = false;
                    is_previous_token_separator = true;
                } else {
                    self.token_flags |= TokenFlags::CONTAINS_INVALID_ESCAPE;
                    if is_previous_token_separator {
                        todo!("error");
                    } else {
                        todo!("error");
                    }
                }
                self.pos += 1;
            } else if ch.is_ascii_digit() {
                allow_separator = true;
                is_previous_token_separator = false;
                self.pos += 1;
            } else {
                break;
            }
        }
        if self.pos > 0 && self.input[self.pos - 1] == b'_' {
            self.token_flags |= TokenFlags::CONTAINS_INVALID_SEPARATOR;
            todo!("error")
        }
    }

    fn scan_digits(&mut self) -> bool {
        let mut is_octal = true;
        loop {
            let Some(ch) = self.ch() else {
                break;
            };
            if !ch.is_ascii_digit() {
                break;
            }
            if !is_octal_digit(ch) {
                is_octal = false;
            }
            self.pos += 1;
        }
        is_octal
    }

    fn scan_number(&mut self) -> Token {
        let start = self.pos;
        if self.input[self.pos] == b'0' {
            self.pos += 1;
            if self.ch() == Some(b'_') {
                self.token_flags |=
                    TokenFlags::CONTAINS_SEPARATOR | TokenFlags::CONTAINS_INVALID_ESCAPE;
                self.push_error(Box::new(errors::NumericSeparatorsAreNotAllowedHere {
                    span: Span::new(self.pos as u32, (self.pos + 1) as u32, self.module_id),
                }));
                self.pos -= 1;
                self.scan_number_fragment()
            } else {
                let is_oct = self.scan_digits();
                if !is_oct {
                    let error = errors::DecimalsWithLeadingZerosAreNotAllowed {
                        span: Span::new(start as u32, self.pos as u32, self.module_id),
                    };
                    self.push_error(Box::new(error));
                } else if start + 1 != self.pos {
                    let fragment = self.input[start + 1..self.pos].to_vec();
                    let help_lit =
                        format!("0o{}", unsafe { String::from_utf8_unchecked(fragment) });
                    let error = errors::OctalLiteralsAreNotAllowed {
                        span: Span::new(start as u32, self.pos as u32, self.module_id),
                        help_lit,
                    };
                    self.push_error(Box::new(error));
                }
            }
        } else {
            self.scan_number_fragment()
        };
        let contain_dot = self.ch() == Some(b'.');
        if contain_dot {
            self.pos += 1;
            self.scan_number_fragment();
            Some(())
        } else {
            None
        };
        let mut end = self.pos;
        let contain_e = self.ch().is_some_and(|c| matches!(c, b'e' | b'E'));
        if contain_e {
            self.pos += 1;
            self.token_flags |= TokenFlags::SCIENTIFIC;
            if self.ch().is_some_and(|c| matches!(c, b'+' | b'-')) {
                self.pos += 1;
            }
            let pre_numeric_part = self.pos;
            self.scan_number_fragment();
            if pre_numeric_part == self.pos {
                self.push_error(Box::new(errors::DigitExpected {
                    span: Span::new(
                        (pre_numeric_part - 1) as u32,
                        self.pos as u32,
                        self.module_id,
                    ),
                }));
            } else {
                // `e{xxx}`
                end = self.pos;
            }
        }

        let result = if self.token_flags.intersects(TokenFlags::CONTAINS_SEPARATOR) {
            debug_assert!(self.input[start..end].contains(&b'_'));
            Cow::Owned(
                self.input[start..end]
                    .iter()
                    .filter(|&&c| c != b'_')
                    .copied()
                    .collect::<Vec<u8>>(),
            )
        } else {
            debug_assert!(!self.input[start..end].contains(&b'_'));
            Cow::Borrowed(&self.input[start..end])
        };

        let kind = if self.ch() == Some(b'n') {
            self.pos += 1;
            let s = unsafe { std::str::from_utf8_unchecked(&result) };
            let value = self.atoms.lock().unwrap().atom(s);
            self.token_value = Some(TokenValue::Ident { value });
            TokenKind::BigInt
        } else {
            let s = unsafe { std::str::from_utf8_unchecked(&result) };
            let num = if contain_dot || contain_e {
                // float
                s.parse::<f64>().unwrap()
            } else {
                parse_integer::<10>(s)
            };
            self.token_value = Some(TokenValue::Number { value: num });
            TokenKind::Number
        };
        let end = self.pos;
        Token::new(kind, Span::new(start as u32, end as u32, self.module_id))
    }

    // From `quickjs/cutils.c/unicode_from_utf8`
    #[cold]
    fn scan_unicode_from_utf8(&mut self, max_len: u32) -> Option<u32> {
        const UTF8_MIN_CODE: [u32; 5] = [0x80, 0x800, 0x10000, 0x00200000, 0x04000000];
        const UTF8_FIRST_CODE_MASK: [u32; 5] = [0x1f, 0xf, 0x7, 0x3, 0x1];

        let mut ch = self.ch_unchecked() as u32;
        assert!(ch >= 0x80);
        let mut offset = 1;
        let l = match ch {
            0xc0..=0xdf => 1,
            0xe0..=0xef => 2,
            0xf0..=0xf7 => 3,
            0xf8..=0xfb => 4,
            0xfc | 0xfd => 5,
            _ => return None,
        };
        if l > (max_len - 1) {
            return None;
        }
        let idx = (l - 1) as usize;
        ch &= UTF8_FIRST_CODE_MASK[idx];
        for _ in 0..l {
            let b = self.input[self.pos + offset] as u32;
            if !(0x80..0xc0).contains(&b) {
                return None;
            }
            offset += 1;
            ch = (ch << 6) | (b & 0x3f);
        }
        if ch < UTF8_MIN_CODE[idx] {
            return None;
        }
        self.pos += offset;
        Some(ch)
    }

    fn scan_identifier(&mut self, ch: u8) -> Option<Token> {
        let start = self.pos;
        let mut first = true;
        loop {
            if first {
                if is_ascii_identifier_start(ch) {
                    self.pos += 1;
                } else if ch < 128 {
                    break;
                } else {
                    let ch = self.scan_unicode_from_utf8(UTF8_CHAR_LEN_MAX)?;
                    if !is_non_ascii_identifier_start(ch, false) {
                        return None;
                    }
                }
                first = false;
            } else if self.pos == self.end() {
                break;
            } else if is_ascii_identifier_part(self.ch_unchecked()) {
                self.pos += 1
            } else if self.ch_unchecked() == b'\\' {
                return self.scan_identifier_slowly(start);
            } else if self.ch_unchecked() < 128 {
                break;
            } else {
                self.scan_unicode_from_utf8(UTF8_CHAR_LEN_MAX)?;
            }
        }
        Some(self.get_ident_token(Cow::Borrowed(&self.input[start..self.pos]), start as u32))
    }

    fn scan_identifier_slowly(&mut self, start: usize) -> Option<Token> {
        debug_assert!(self.pos > start);
        let mut result = self.input[start..self.pos].to_vec();
        loop {
            if self.pos == self.end() {
                break;
            } else if is_ascii_identifier_part(self.ch_unchecked()) {
                self.pos += 1;
            } else if self.ch_unchecked() == b'\\' {
                result.extend(self.scan_identifier_parts());
            } else if self.ch_unchecked() < 128 {
                break;
            } else {
                self.scan_unicode_from_utf8(UTF8_CHAR_LEN_MAX)?;
            }
        }
        Some(self.get_ident_token(Cow::Owned(result), start as u32))
    }

    fn scan_comment_directive_kind(&mut self) {
        assert_eq!(self.ch_unchecked(), b'@');
        let start = self.pos as u32;
        if self.next_content_is(b"@ts-expect-error") {
            self.comment_directives.push(CommentDirective {
                line: self.line as u32,
                range: Span::new(start, self.pos as u32, self.module_id),
                kind: CommentDirectiveKind::ExpectError,
            });
        } else if self.next_content_is(b"@ts-ignore") {
            self.comment_directives.push(CommentDirective {
                line: self.line as u32,
                range: Span::new(start, self.pos as u32, self.module_id),
                kind: CommentDirectiveKind::Ignore,
            });
        } else {
            self.pos += 1;
        }
    }

    pub(super) fn next_content_is(&mut self, s: &[u8]) -> bool {
        if self.pos + s.len() > self.end() {
            return false;
        }
        for (i, c) in s.iter().enumerate() {
            if c != unsafe { self.input.get_unchecked(self.pos + i) } {
                return false;
            }
        }
        self.pos += s.len();
        true
    }

    fn scan_exact_number_of_hex_digits(&mut self, count: u8, can_have_sep: bool) -> Option<u32> {
        let s = self.scan_hex_digits(count as usize, false, can_have_sep);
        let s = unsafe { str::from_utf8_unchecked(&s) };
        u32::from_str_radix(s, 16).ok()
    }

    fn peek_extend_unicode_escape(&mut self) -> Option<u32> {
        debug_assert!(self.ch_unchecked() == b'\\');
        if self.next_ch() == Some(b'u') && self.next_next_ch() == Some(b'{') {
            let start = self.pos;
            self.pos += 3;
            let s = self.scan_minimum_number_of_hex_digits(1, false);
            let s = unsafe { str::from_utf8_unchecked(&s) };
            let v = u32::from_str_radix(s, 16).ok();
            self.pos = start;
            v
        } else {
            None
        }
    }

    fn peek_unicode_escape(&mut self) -> Option<u32> {
        debug_assert!(self.ch_unchecked() == b'\\');
        if self.pos + 5 < self.end() && self.next_ch_unchecked() == b'u' {
            let start = self.pos;
            self.pos += 2;
            let v = self.scan_exact_number_of_hex_digits(4, false);
            self.pos = start;
            v
        } else {
            None
        }
    }

    pub(super) fn next_token(&mut self) {
        if self.token.kind.is_keyword() && self.token_flags.intersects(TokenFlags::UNICODE_ESCAPE) {
            let error = errors::KeywordsCannotContainEscapeCharacters {
                span: self.token.span,
            };
            self.push_error(Box::new(error));
        }
        self.next_token_without_checked();
    }

    pub(super) fn next_token_without_checked(&mut self) {
        self.full_start_pos = self.pos;
        self.token_flags = TokenFlags::empty();
        let mut start;
        loop {
            start = self.pos;
            if self.pos == self.end() {
                self.token = Token::new(
                    TokenKind::EOF,
                    Span::new(start as u32, start as u32, self.module_id),
                );
                return;
            }
            let ch = self.ch_unchecked();
            if self.pos == 0 && ch == b'#' {
                // TODO: Handle shebang
            }
            let token = match ch {
                b'/' => {
                    if self.next_ch() == Some(b'/') && self.next_next_ch() == Some(b'/') {
                        // `///`
                        self.pos += 3;
                        while self.pos < self.end() && !is_line_break(self.ch_unchecked()) {
                            let ch = self.ch_unchecked();
                            if ch == b'<' {
                                self.scan_triple_slash_xml_pragma();
                            } else {
                                self.pos += 1;
                            }
                        }
                        continue;
                    } else if self.next_ch() == Some(b'/') {
                        // `//`
                        self.pos += 2;
                        let mut only_has_ascii_whitespace = true;
                        while self.pos < self.end() && !is_line_break(self.ch_unchecked()) {
                            let ch = self.ch_unchecked();
                            if ch != b'@' && !ch.is_ascii_whitespace() {
                                only_has_ascii_whitespace = false;
                                self.pos += 1;
                            } else if only_has_ascii_whitespace && ch == b'@' {
                                self.scan_comment_directive_kind();
                            } else {
                                self.pos += 1;
                            }
                        }
                        let c = bolt_ts_ast::SingleLineComment {
                            span: Span::new(start as u32, self.pos as u32, self.module_id),
                        };
                        self.comments.push(bolt_ts_ast::Comment::SingleLine(c));
                        continue;
                    } else if self.next_ch() == Some(b'*') {
                        // `/*`
                        self.pos += 2;
                        while self.pos < self.end() {
                            if self.ch_unchecked() == b'*' && self.next_ch() == Some(b'/') {
                                self.pos += 2;
                                break;
                            } else {
                                self.pos += 1;
                            }
                        }
                        let c = bolt_ts_ast::MultiLineComment {
                            span: Span::new(start as u32, self.pos as u32, self.module_id),
                        };
                        self.comments.push(bolt_ts_ast::Comment::MultiLine(c));
                        continue;
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::SlashEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Slash,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'%' => {
                    if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::PercentEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Percent,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'=' => {
                    if self.next_ch() == Some(b'>') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::EqGreat,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else if self.next_ch() == Some(b'=') {
                        if self.next_next_ch() == Some(b'=') {
                            self.pos += 3;
                            Token::new(
                                TokenKind::EqEqEq,
                                Span::new(start as u32, self.pos as u32, self.module_id),
                            )
                        } else {
                            self.pos += 2;
                            Token::new(
                                TokenKind::EqEq,
                                Span::new(start as u32, self.pos as u32, self.module_id),
                            )
                        }
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Eq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'+' => {
                    if self.next_ch() == Some(b'+') {
                        // ++
                        self.pos += 2;
                        Token::new(
                            TokenKind::PlusPlus,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::PlusEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Plus,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'-' => {
                    if self.next_ch() == Some(b'-') {
                        // --
                        self.pos += 2;
                        Token::new(
                            TokenKind::MinusMinus,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::MinusEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Minus,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'*' => {
                    if self.next_ch() == Some(b'*') {
                        if self.next_next_ch() == Some(b'=') {
                            // **=
                            self.pos += 3;
                            Token::new(
                                TokenKind::AsteriskAsteriskEq,
                                Span::new(start as u32, self.pos as u32, self.module_id),
                            )
                        } else {
                            // **
                            self.pos += 2;
                            Token::new(
                                TokenKind::AsteriskAsterisk,
                                Span::new(start as u32, self.pos as u32, self.module_id),
                            )
                        }
                    } else if self.next_ch() == Some(b'=') {
                        // *=
                        self.pos += 2;
                        Token::new(
                            TokenKind::AsteriskEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        // *
                        self.pos += 1;
                        Token::new(
                            TokenKind::Asterisk,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'|' => {
                    if self.next_ch() == Some(b'|') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::PipePipe,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::PipeEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Pipe,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'?' => {
                    let kind = if self.next_ch() == Some(b'.')
                        && self.next_next_ch().is_none_or(|c| !c.is_ascii_digit())
                    {
                        self.pos += 2;
                        TokenKind::QuestionDot
                    } else {
                        self.pos += 1;
                        TokenKind::Question
                    };
                    Token::new(
                        kind,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'&' => {
                    if self.next_ch() == Some(b'&') {
                        // &&
                        self.pos += 2;
                        Token::new(
                            TokenKind::AmpAmp,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else if self.next_ch() == Some(b'=') {
                        // &=
                        self.pos += 2;
                        Token::new(
                            TokenKind::AmpEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Amp,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'<' => {
                    if self.next_ch() == Some(b'<') {
                        if self.next_next_ch() == Some(b'=') {
                            // <<=
                            self.pos += 3;
                            Token::new(
                                TokenKind::LessLessEq,
                                Span::new(start as u32, self.pos as u32, self.module_id),
                            )
                        } else {
                            // <<
                            self.pos += 2;
                            Token::new(
                                TokenKind::LessLess,
                                Span::new(start as u32, self.pos as u32, self.module_id),
                            )
                        }
                    } else if self.next_ch() == Some(b'=') {
                        // <=
                        self.pos += 2;
                        Token::new(
                            TokenKind::LessEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Less,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'^' if self.next_ch() == Some(b'=') => {
                    self.pos += 2;
                    Token::new(
                        TokenKind::CaretEq,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'.' => {
                    let next = self.next_ch();
                    if next.is_some_and(|ch| ch.is_ascii_digit()) {
                        // .123
                        self.scan_number()
                    } else if next == Some(b'.') && self.next_next_ch() == Some(b'.') {
                        // ...
                        self.pos += 3;
                        Token::new(
                            TokenKind::DotDotDot,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        // .
                        self.pos += 1;
                        Token::new(
                            TokenKind::Dot,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'!' if self.next_ch() == Some(b'=') => {
                    let kind = if self.next_next_ch() == Some(b'=') {
                        self.pos += 3;
                        TokenKind::BangEqEq
                    } else {
                        self.pos += 2;
                        TokenKind::BangEq
                    };

                    let span = Span::new(start as u32, self.pos as u32, self.module_id);
                    Token::new(kind, span)
                }
                b'>' | b',' | b';' | b':' | b'[' | b']' | b'(' | b')' | b'{' | b'}' | b'!'
                | b'~' | b'^' => {
                    self.pos += 1;
                    let kind = unsafe { std::mem::transmute::<u8, TokenKind>(ch) };
                    Token::new(
                        kind,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'\'' | b'"' => {
                    let (v, key_value) = self.scan_string(ch, false);
                    let len = v.len();
                    let atom = self
                        .atoms
                        .lock()
                        .unwrap()
                        .atom(unsafe { str::from_utf8_unchecked(&v) });
                    if len != key_value.len() {
                        let key_atom = self
                            .atoms
                            .lock()
                            .unwrap()
                            .atom(unsafe { str::from_utf8_unchecked(&key_value) });
                        self.string_key_value = Some(key_atom);
                    } else {
                        self.string_key_value = Some(atom);
                    };
                    self.token_value = Some(TokenValue::Ident { value: atom });
                    Token::new(
                        TokenKind::String,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'`' => self.scan_template_and_set_token_value(false),
                b'0' if self.pos + 2 < self.end()
                    && self.next_ch().is_some_and(|c| matches!(c, b'X' | b'x')) =>
                {
                    self.pos += 2;
                    let v = self.scan_minimum_number_of_hex_digits(1, true);
                    if v.is_empty() {
                        todo!("throw error")
                    }
                    self.token_flags = TokenFlags::HEX_SPECIFIER;
                    let s = unsafe { str::from_utf8_unchecked(&v) };
                    // TODO: check bigint suffix
                    let v = parse_integer::<16>(s);
                    self.token_value = Some(TokenValue::Number { value: v });
                    Token::new(
                        TokenKind::Number,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'0' if self.pos + 2 < self.end()
                    && self.next_ch().is_some_and(|c| matches!(c, b'B' | b'b')) =>
                {
                    self.pos += 2;
                    let v = self.scan_binary_or_octal_digits(8);
                    if v.is_empty() {
                        todo!()
                    }
                    self.token_flags = TokenFlags::BINARY_SPECIFIER;
                    let s = unsafe { str::from_utf8_unchecked(&v) };
                    let v = parse_integer::<2>(s);
                    // TODO: check bigint suffix
                    self.token_value = Some(TokenValue::Number { value: v });
                    Token::new(
                        TokenKind::Number,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'0' if self.pos + 2 < self.end()
                    && self.next_ch().is_some_and(|c| matches!(c, b'O' | b'o')) =>
                {
                    self.pos += 2;
                    let v = self.scan_binary_or_octal_digits(8);
                    if v.is_empty() {
                        todo!()
                    }
                    self.token_flags = TokenFlags::OCTAL_SPECIFIER;
                    let s = unsafe { str::from_utf8_unchecked(&v) };
                    // TODO: check bigint suffix
                    let v = parse_integer::<8>(s);
                    self.token_value = Some(TokenValue::Number { value: v });
                    Token::new(
                        TokenKind::Number,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'0'..=b'9' => self.scan_number(),
                b'\\' => {
                    if let Some(extended_cooked_char) = self.peek_extend_unicode_escape()
                        && is_identifier_start(extended_cooked_char, false) {
                            let mut unicode = self.scan_extended_unicode_escape(true);
                            let ident_parts = self.scan_identifier_parts();
                            unicode.extend(ident_parts);
                            self.token = self.get_ident_token(Cow::Owned(unicode), start as u32);
                            return;
                        }

                    if let Some(cooked_char) = self.peek_unicode_escape()
                        && is_identifier_start(cooked_char, false) {
                            self.pos += 6;
                            self.token_flags |= TokenFlags::UNICODE_ESCAPE;
                            let ch = unsafe {
                                // SAFETY: checked in `peek_unicode_escape`
                                std::char::from_u32_unchecked(cooked_char)
                            };
                            let mut s = ch.to_string().into_bytes();
                            let ident_parts = self.scan_identifier_parts();
                            s.extend(ident_parts);
                            self.token = self.get_ident_token(Cow::Owned(s), start as u32);
                            return;
                        }

                    self.push_error(Box::new(errors::InvalidCharacter {
                        span: Span::new(start as u32, self.pos as u32 + 1, self.module_id),
                    }));
                    self.pos += 1;
                    Token::new(
                        TokenKind::Unknown,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                _ if ch.is_ascii_whitespace() => {
                    self.pos += 1;
                    // TODO: \r\n
                    if ch == b'\n' {
                        self.token_flags.insert(TokenFlags::PRECEDING_LINE_BREAK);
                        self.line += 1;
                        self.record_new_line_offset();
                    }
                    continue;
                }
                _ => match self.scan_identifier(ch) {
                    Some(token) => token,
                    None => {
                        let span = Span::new(start as u32, self.pos as u32, self.module_id);
                        self.push_error(Box::new(errors::InvalidCharacter { span }));
                        Token::new(TokenKind::Unknown, span)
                    }
                },
            };
            self.token = token;
            break;
        }
    }

    pub(super) fn record_new_line_offset(&mut self) {
        // it means there has useless scan when this condition not true,
        // how can we use `self.line_map.push(xxx)` directly?
        if self
            .line_map
            .last()
            .copied()
            .is_none_or(|pos| pos < self.line_start as u32)
        {
            self.line_map.push(self.line_start as u32);
            self.line_start = self.pos;
        }
    }

    fn scan_template_and_set_token_value(
        &mut self,
        should_emit_invalid_escape_error: bool,
    ) -> Token {
        let started_with_backtick = self.ch_unchecked() == b'`';
        let start = self.pos;
        self.pos += 1;
        let mut contents = Vec::with_capacity(32);
        let kind = loop {
            if self.pos == self.end() {
                return Token::new(
                    TokenKind::EOF,
                    Span::new(start as u32, start as u32, self.module_id),
                );
            }
            let ch = self.ch_unchecked();
            if ch == b'`' {
                self.pos += 1;
                let s = unsafe { str::from_utf8_unchecked(&contents) };
                let atom = self.atoms.lock().unwrap().atom(s);
                self.token_value = Some(TokenValue::Ident { value: atom });
                break if started_with_backtick {
                    TokenKind::NoSubstitutionTemplate
                } else {
                    TokenKind::TemplateTail
                };
            } else if ch == b'$' && self.next_ch() == Some(b'{') {
                // `${`
                let s = unsafe { str::from_utf8_unchecked(&contents) };
                let atom = self.atoms.lock().unwrap().atom(s);
                self.token_value = Some(TokenValue::Ident { value: atom });
                self.pos += 2;
                break if started_with_backtick {
                    TokenKind::TemplateHead
                } else {
                    TokenKind::TemplateMiddle
                };
            } else {
                contents.push(ch);
                self.pos += 1;
            }
        };
        Token::new(
            kind,
            Span::new(start as u32, self.pos as u32, self.module_id),
        )
    }

    fn scan_escape_sequence(&mut self, flags: EscapeSequenceScanningFlags) -> Vec<u8> {
        assert_eq!(self.ch_unchecked(), b'\\');
        let start = self.pos;
        self.pos += 1;
        let end = self.end();
        if self.pos >= end {
            // TODO: error
            return vec![];
        }
        let ch = self.ch_unchecked();
        self.pos += 1;
        if ch == b'0' && (self.pos >= end || !self.ch_unchecked().is_ascii_digit()) {
            return vec![b'\0'];
        }
        if matches!(ch, b'0'..=b'3') && self.pos < end && is_octal_digit(self.ch_unchecked()) {
            self.pos += 1;
        }

        if matches!(ch, b'0'..=b'7') {
            if self.pos < end && is_octal_digit(self.ch_unchecked()) {
                self.pos += 1;
            }
            self.token_flags |= TokenFlags::CONTAINS_SEPARATOR;
            if flags.intersects(EscapeSequenceScanningFlags::REPORT_INVALID_ESCAPE_ERRORS) {
                todo!()
            }
            return self.input[start..self.pos].to_vec();
        }
        if matches!(ch, b'8' | b'9') {
            self.token_flags |= TokenFlags::CONTAINS_SEPARATOR;
            if flags.intersects(EscapeSequenceScanningFlags::REPORT_INVALID_ESCAPE_ERRORS) {
                todo!()
            }
            return self.input[start..self.pos].to_vec();
        }
        match ch {
            b'b' => vec![8],
            b't' => vec![9],
            b'n' => vec![b'\n'],
            b'v' => vec![11],
            b'f' => vec![12],
            b'r' => vec![b'\r'],
            b'\'' => vec![b'\''],
            b'"' => vec![b'"'],
            b'u' => {
                if self.pos < end && self.ch_unchecked() == b'{' {
                    self.pos -= 2;
                    let result = self.scan_extended_unicode_escape(
                        flags.intersects(EscapeSequenceScanningFlags::REPORT_INVALID_ESCAPE_ERRORS),
                    );
                    if !flags.intersects(EscapeSequenceScanningFlags::ALLOW_EXTENDED_UNICODE_ESCAPE)
                    {
                        self.token_flags |= TokenFlags::CONTAINS_INVALID_ESCAPE;
                        if flags
                            .intersects(EscapeSequenceScanningFlags::REPORT_INVALID_ESCAPE_ERRORS)
                        {
                            // TODO: handle error
                        }
                    }
                    return result;
                }
                // TODO: more case
                vec![ch]
            }
            b'\r' => {
                if self.pos < end && self.ch_unchecked() == b'\n' {
                    self.pos += 1;
                }
                vec![]
            }
            b'\n' => vec![],
            // TODO: more case
            _ => {
                if flags.intersects(EscapeSequenceScanningFlags::ANY_UNICODE_MODE)
                    || flags.intersects(EscapeSequenceScanningFlags::REGULAR_EXPRESSION)
                        && !flags.intersects(EscapeSequenceScanningFlags::ANNEX_B)
                        && is_ascii_identifier_start(ch)
                {
                    todo!("error handle")
                }
                vec![ch]
            }
        }
    }

    fn scan_extended_unicode_escape(&mut self, should_emit_invalid_escape_error: bool) -> Vec<u8> {
        let start = self.pos;
        assert_eq!(self.ch_unchecked(), b'\\');
        assert_eq!(self.next_ch(), Some(b'u'));
        assert_eq!(self.next_next_ch(), Some(b'{'));
        self.pos += 3;
        // let escaped_start = self.pos;
        let escaped_value_string = self.scan_minimum_number_of_hex_digits(1, false);
        let escape_value = u32::from_str_radix(
            unsafe { core::str::from_utf8_unchecked(&escaped_value_string) },
            16,
        );
        let mut is_invalid_extended_escape = false;
        match escape_value {
            Ok(escape_value) => {
                if escape_value > 0x10ffff {
                    if should_emit_invalid_escape_error {
                        todo!()
                    }
                    is_invalid_extended_escape = true;
                }
            }
            Err(_) => {
                if should_emit_invalid_escape_error {
                    todo!()
                }
                is_invalid_extended_escape = true
            }
        }

        if self.pos >= self.end() {
            if should_emit_invalid_escape_error {
                todo!()
            }
            is_invalid_extended_escape = true;
        } else if self.ch_unchecked() == b'}' {
            self.pos += 1;
        } else {
            if should_emit_invalid_escape_error {
                todo!()
            }
            is_invalid_extended_escape = true;
        }

        if is_invalid_extended_escape {
            self.token_flags |= TokenFlags::CONTAINS_INVALID_ESCAPE;
            self.input[start..self.pos].to_vec()
        } else {
            self.token_flags |= TokenFlags::EXTENDED_UNICODE_ESCAPE;
            utf16_encode_as_bytes(escape_value.unwrap())
        }
    }

    #[inline]
    fn scan_minimum_number_of_hex_digits(&mut self, count: usize, can_have_sep: bool) -> Vec<u8> {
        self.scan_hex_digits(count, true, can_have_sep)
    }

    fn scan_hex_digits(
        &mut self,
        min_count: usize,
        scan_as_many_as_possible: bool,
        can_have_sep: bool,
    ) -> Vec<u8> {
        let mut value_chars = Vec::with_capacity(8);
        let mut allow_sep = false;
        let mut is_prev_token_sep = false;
        while value_chars.len() < min_count || scan_as_many_as_possible {
            let Some(mut ch) = self.ch() else {
                break;
            };
            if can_have_sep && ch == b'_' {
                if allow_sep {
                    allow_sep = false;
                    is_prev_token_sep = true;
                } else if is_prev_token_sep {
                    todo!()
                } else {
                    todo!()
                }
                self.pos += 1;
                continue;
            }
            allow_sep = can_have_sep;
            if matches!(ch, b'A'..=b'F') {
                ch += b'a' - b'A';
            } else if !matches!(ch, b'0'..=b'9' | b'a'..=b'f') {
                break;
            }
            value_chars.push(ch);
            self.pos += 1;
            is_prev_token_sep = false;
        }
        if value_chars.len() < min_count {
            value_chars.clear();
        }
        if self.input[self.pos - 1] == b'_' {
            todo!()
        };
        value_chars
    }

    pub(super) fn scan_string(
        &mut self,
        quote: u8,
        jsx_attribute_string: bool,
    ) -> (Vec<u8>, Vec<u8>) {
        assert_eq!(self.ch_unchecked(), quote);
        self.pos += 1;
        let mut v = Vec::with_capacity(32);
        let mut prev = 0;
        let mut key = Vec::with_capacity(32);
        loop {
            if self.pos >= self.end() {
                self.token_flags |= TokenFlags::UNTERMINATED;
                self.push_error(Box::new(errors::UnterminatedStringLiteral {
                    span: Span::new(self.token.end(), self.pos as u32, self.module_id),
                }));
                break;
            }
            let ch = self.ch_unchecked();
            if ch == quote {
                self.pos += 1;
                break;
            } else if ch == b'\\' && !jsx_attribute_string {
                let t = self.scan_escape_sequence(
                    EscapeSequenceScanningFlags::STRING
                        .union(EscapeSequenceScanningFlags::REPORT_ERRORS),
                );
                v.extend(t.iter());
                key.extend(t.iter());
                continue;
            }

            if !(ch == b'\n' && prev == b'\\') {
                key.push(ch);
            } else {
                key.pop();
            }
            self.pos += 1;
            v.push(ch);
            prev = ch;
        }
        (v, key)
    }

    pub(super) fn re_scan_greater(&mut self) -> TokenKind {
        if self.token.kind != TokenKind::Great {
            return self.token.kind;
        }
        let start = self.token.start();
        assert_eq!(start + 1, self.pos as u32);
        self.token = if self.ch_unchecked() == b'>' {
            // >>
            if self.next_ch() == Some(b'>') {
                // >>>
                if self.next_next_ch() == Some(b'=') {
                    // >>>=
                    self.pos += 3;
                    Token::new(
                        TokenKind::GreatGreatGreatEq,
                        Span::new(start, self.pos as u32, self.module_id),
                    )
                } else {
                    // >>>
                    self.pos += 2;
                    Token::new(
                        TokenKind::GreatGreatGreat,
                        Span::new(start, self.pos as u32, self.module_id),
                    )
                }
            } else if self.next_ch() == Some(b'=') {
                // >>=
                self.pos += 2;
                Token::new(
                    TokenKind::GreatGreatEq,
                    Span::new(start, self.pos as u32, self.module_id),
                )
            } else {
                // >>
                self.pos += 1;
                Token::new(
                    TokenKind::GreatGreat,
                    Span::new(start, self.pos as u32, self.module_id),
                )
            }
        } else if self.ch_unchecked() == b'=' {
            // >=
            self.pos += 1;
            Token::new(
                TokenKind::GreatEq,
                Span::new(start, self.pos as u32, self.module_id),
            )
        } else {
            // >
            self.token
        };
        self.token.kind
    }

    pub(super) fn re_scan_less(&mut self) -> TokenKind {
        if self.token.kind == TokenKind::LessLess {
            let token_start = self.token.start();
            self.pos = token_start as usize + 1;
            self.token = Token::new(
                TokenKind::Less,
                Span::new(token_start, self.pos as u32, self.module_id),
            );
        }
        self.token.kind
    }

    pub(super) fn re_scan_template_token(&mut self, is_tagged_template: bool) -> Token {
        self.pos = self.token.start() as usize;
        self.token = self.scan_template_and_set_token_value(!is_tagged_template);
        self.token
    }

    pub(super) fn re_scan_slash_token(&mut self, report_error: bool) {
        use TokenKind::*;
        if !matches!(self.token.kind, Slash | SlashEq) {
            return;
        }

        let start = self.token.start();
        let start_of_regexp_body = start + 1;
        self.pos = start_of_regexp_body as usize;
        let mut in_escape = false;
        let mut named_capture_groups = false;
        let mut in_character_class = false;
        loop {
            let Some(ch) = self.ch() else {
                self.token_flags |= TokenFlags::UNTERMINATED;
                break;
            };
            if is_line_break(ch) {
                self.token_flags |= TokenFlags::UNTERMINATED;
                break;
            };
            if in_escape {
                in_escape = false;
            } else if ch == b'/' && !in_character_class {
                break;
            } else if ch == b'[' {
                in_character_class = true;
            } else if ch == b'\\' {
                in_escape = true;
            } else if ch == b']' {
                in_character_class = false;
            } else if !in_character_class
                && ch == b'('
                && self.input.get(self.pos + 1).is_some_and(|c| b'?'.eq(c))
                && self.input.get(self.pos + 2).is_some_and(|c| b'<'.eq(c))
                && self
                    .input
                    .get(self.pos + 3)
                    .is_some_and(|c| !matches!(c, b'=' | b'!'))
            {
                named_capture_groups = true;
            }
            self.pos += 1;
        }
        let end_of_regexp_body = self.pos;
        if self.token_flags.intersects(TokenFlags::UNTERMINATED) {
            self.pos = start_of_regexp_body as usize;
            in_escape = false;
            let mut character_class_depth = 0;
            let mut in_decimal_quantifier = false;
            let mut group_depth = 0;
            while self.pos < end_of_regexp_body {
                let ch = self.ch_unchecked();
                if in_escape {
                    in_escape = false;
                } else if ch == b'\\' {
                    in_escape = true;
                } else if ch == b'[' {
                    character_class_depth += 1;
                } else if ch == b']' && character_class_depth > 0 {
                    character_class_depth -= 1;
                } else if character_class_depth == 0 {
                    if ch == b'{' {
                        in_decimal_quantifier = true;
                    } else if ch == b'}' && in_decimal_quantifier {
                        in_decimal_quantifier = false;
                    } else if !in_decimal_quantifier {
                        if ch == b'(' {
                            group_depth += 1;
                        } else if ch == b')' && group_depth > 0 {
                            group_depth -= 1;
                        } else if matches!(ch, b')' | b']' | b'}') {
                            break;
                        }
                    }
                }
                self.pos += 1;
            }
            while let Some(prev) = self.input.get(self.pos - 1).copied() {
                if prev == b';' || prev.is_ascii_whitespace() {
                    self.pos -= 1;
                } else if !prev.is_ascii() {
                    todo!("unicode in regexp")
                } else {
                    break;
                }
            }
            // TODO: unterminated_regexp_expr;
        } else {
            self.pos += 1;
            let mut regexp_flags = RegularExpressionFlags::empty();
            while let Some(ch) = self.ch() {
                if !is_ascii_identifier_part(ch) {
                    break;
                }
                if !ch.is_ascii() {
                    todo!("unicode in regexp")
                }
                if report_error {
                    let flags = ch_to_regexp_flags(ch);
                    if let Some(flags) = flags {
                        if regexp_flags.intersects(flags) {
                            todo!("duplicate regexp flags")
                        } else if (regexp_flags | flags)
                            .contains(RegularExpressionFlags::ANY_UNICODE_MODE)
                        {
                            todo!("u and v flags are mutually exclusive")
                        } else {
                            regexp_flags |= flags;
                            todo!("check language version");
                        }
                    } else {
                        todo!("invalid regexp flags")
                    }
                }
                self.pos += 1;
            }
        }
        let atom = self
            .atoms
            .lock()
            .unwrap()
            .atom(unsafe { str::from_utf8_unchecked(&self.input[start as usize..self.pos]) });
        self.token_value = Some(TokenValue::Ident { value: atom });
        self.token = Token::new(
            TokenKind::Regexp,
            Span::new(start, end_of_regexp_body as u32, self.module_id),
        );
    }

    pub(super) fn re_scan_jsx_token(&mut self, allow_multiline_jsx_text: bool) {
        use TokenKind::*;
        if matches!(self.token.kind, JSXText | JSXTextAllWhiteSpaces) {
            return;
        }
        self.pos = self.full_start_pos;
        self.scan_jsx_token(allow_multiline_jsx_text);
    }

    pub(super) fn scan_jsx_token(&mut self, allow_multiline_jsx_text: bool) {
        self.full_start_pos = self.pos;
        let token_start = self.pos;

        if self.pos >= self.end() {
            self.token = Token::new(
                TokenKind::EOF,
                Span::new(token_start as u32, token_start as u32, self.module_id),
            );
            return;
        }

        let ch = self.ch_unchecked();
        if ch == b'<' {
            if self.next_ch() == Some(b'/') {
                self.pos += 2;
                self.token = Token::new(
                    TokenKind::LessSlash,
                    Span::new(token_start as u32, self.pos as u32, self.module_id),
                );
            } else {
                self.pos += 1;
                self.token = Token::new(
                    TokenKind::Less,
                    Span::new(token_start as u32, self.pos as u32, self.module_id),
                );
            }
            return;
        } else if ch == b'{' {
            self.pos += 1;
            self.token = Token::new(
                TokenKind::LBrace,
                Span::new(token_start as u32, self.pos as u32, self.module_id),
            );
            return;
        }

        let mut first_non_whitespace = 0;

        while let Some(ch) = self.ch() {
            if ch == b'{' {
                break;
            } else if ch == b'<' {
                // TODO: is_conflict_mark_trivia
                break;
            } else if ch == b'>' {
                let error = errors::UnexpectedTokenDidYouMeanOrGt {
                    span: Span::new(self.pos as u32, (self.pos + 1) as u32, self.module_id),
                };
                self.push_error(Box::new(error));
            } else if ch == b'}' {
                let error = errors::UnexpectedTokenDidYouMeanOrRBrace {
                    span: Span::new(token_start as u32, self.pos as u32, self.module_id),
                };
                self.push_error(Box::new(error));
            }

            let is_line_break = is_line_break(ch);
            if is_line_break && first_non_whitespace == 0 {
                first_non_whitespace = -1;
            } else if !allow_multiline_jsx_text && is_line_break && first_non_whitespace > 0 {
                break;
            } else if ch.is_ascii_whitespace() {
                first_non_whitespace = self.pos as isize;
            }

            self.pos += 1;
        }

        let s = unsafe { str::from_utf8_unchecked(&self.input[token_start..self.pos]) };
        let atom = self.atoms.lock().unwrap().atom(s);
        self.token_value = Some(TokenValue::Ident { value: atom });

        let token_kind = if first_non_whitespace == -1 {
            TokenKind::JSXTextAllWhiteSpaces
        } else {
            TokenKind::JSXText
        };
        self.token = Token::new(
            token_kind,
            Span::new(token_start as u32, self.pos as u32, self.module_id),
        )
    }

    fn get_ident_token(&mut self, ident: Cow<[u8]>, start: u32) -> Token {
        let len = ident.len();

        let s = unsafe { str::from_utf8_unchecked(&ident) };
        let id = self.atoms.lock().unwrap().atom(s);
        if len > 1 && len < 13 {
            let first = *unsafe { ident.get_unchecked(0) };
            if first.is_ascii_lowercase()
                && let Some(kind) = atom_to_token(id) {
                    debug_assert!(!self.atoms.lock().unwrap().get(id).is_empty());
                    let span = Span::new(start, self.pos as u32, self.module_id);
                    self.token_value = Some(TokenValue::Ident { value: id });
                    return Token::new(kind, span);
                }
        }
        self.token_value = Some(TokenValue::Ident { value: id });
        Token::new(
            TokenKind::Ident,
            Span::new(start, self.pos as u32, self.module_id),
        )
    }

    pub(super) fn scan_jsx_ident(&mut self) {
        if !self.token.kind.is_ident_or_keyword() {
            return;
        }

        let mut v = Vec::with_capacity(16);
        while let Some(ch) = self.ch() {
            if ch == b'-' {
                v.push(ch);
                self.pos += 1;
                continue;
            } else {
                let old_pos = self.pos;
                v.extend(self.scan_identifier_parts());
                if self.pos == old_pos {
                    break;
                }
            }
        }

        if !v.is_empty() {
            let token_value = self.ident_token();
            debug_assert!(!self.atoms.lock().unwrap().get(token_value).is_empty());
            let mut token_value = self
                .atoms
                .lock()
                .unwrap()
                .get(token_value)
                .as_bytes()
                .to_vec();
            token_value.extend(v);
            self.token = self.get_ident_token(Cow::Owned(token_value), self.token.start());
        } else {
            debug_assert!(self.ident_token() != keyword::IDENT_EMPTY);
        }
    }

    fn scan_identifier_parts(&mut self) -> Vec<u8> {
        let mut result = Vec::with_capacity(32);
        let mut start = self.pos;
        while let Some(ch) = self.ch() {
            if is_ascii_identifier_part(ch) {
                self.pos += 1;
            } else if ch == b'\\' {
                let ch = self.peek_extend_unicode_escape();
                if let Some(ch) = ch
                    && is_identifier_part(ch, false) {
                        result.extend(self.scan_extended_unicode_escape(true));
                        start = self.pos;
                        continue;
                    }
                let ch = self.peek_unicode_escape();
                if !(ch.is_some_and(|ch| is_identifier_part(ch, false))) {
                    break;
                }
                self.token_flags |= TokenFlags::UNICODE_ESCAPE;
                result.extend(self.input[start..self.pos].iter());
                let ch = unsafe {
                    // SAFETY: checked in `peek_unicode_escape`
                    std::char::from_u32_unchecked(ch.unwrap())
                };
                result.extend(ch.to_string().into_bytes());
                self.pos += 6; // \u{xxxxxx}
                start = self.pos;
            } else {
                // TODO: unicode identifier part
                break;
            }
        }
        result.extend_from_slice(&self.input[start..self.pos]);
        result
    }

    pub(super) fn scan_jsx_attr_value(&mut self) {
        self.full_start_pos = self.pos;
        let start = self.pos;
        let ch = self.ch();
        match self.ch() {
            Some(b'\'') | Some(b'"') => {
                let (value, _) = self.scan_string(unsafe { ch.unwrap_unchecked() }, true);
                self.token_value = Some(TokenValue::Ident {
                    value: self
                        .atoms
                        .lock()
                        .unwrap()
                        .atom(unsafe { str::from_utf8_unchecked(&value) }),
                });
                self.token = Token::new(
                    TokenKind::String,
                    Span::new(start as u32, self.pos as u32, self.module_id),
                );
            }
            _ => self.next_token(),
        }
    }
}

fn ch_to_regexp_flags(ch: u8) -> Option<RegularExpressionFlags> {
    let flags = match ch {
        b'd' => RegularExpressionFlags::HAS_INDICES,
        b'g' => RegularExpressionFlags::GLOBAL,
        b'i' => RegularExpressionFlags::IGNORE_CASE,
        b'm' => RegularExpressionFlags::MULTILINE,
        b's' => RegularExpressionFlags::DOT_ALL,
        b'u' => RegularExpressionFlags::UNICODE,
        b'v' => RegularExpressionFlags::UNICODE_SETS,
        b'y' => RegularExpressionFlags::STICKY,
        _ => return None,
    };
    Some(flags)
}

fn utf16_encode_as_bytes(code_point: u32) -> Vec<u8> {
    assert!(code_point <= 0x10FFFF);
    if code_point < 256 {
        return vec![code_point as u8];
    } else if code_point <= 0xFFFF {
        let lo = (code_point & 0xFF) as u8;
        let hi = ((code_point >> 8) & 0xFF) as u8;
        return vec![lo, hi];
    }

    let surrogate = code_point - 0x10000;
    let high_surrogate = ((surrogate >> 10) + 0xD800) as u16;
    let low_surrogate = ((surrogate & 0x3FF) + 0xDC00) as u16;

    let mut buf = Vec::with_capacity(4);
    buf.extend_from_slice(&high_surrogate.to_le_bytes());
    buf.extend_from_slice(&low_surrogate.to_le_bytes());
    buf
}

#[test]
fn test_utf16_encode_as_bytes() {
    assert_eq!(utf16_encode_as_bytes(9), vec![9]);
    assert_eq!(utf16_encode_as_bytes(20), vec![20]);
    assert_eq!(utf16_encode_as_bytes(255), vec![255]);
}
