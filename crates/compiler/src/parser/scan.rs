use std::borrow::Cow;

use bolt_ts_span::Span;

use super::token::{keyword_idx_to_token, Token, TokenFlags, TokenKind};
use super::{PResult, ParserState, TokenValue};

use crate::atoms::AtomId;
use crate::keyword::KEYWORDS;

#[inline(always)]
fn is_ascii_letter(ch: u8) -> bool {
    ch.is_ascii_alphabetic()
}

#[inline(always)]
fn is_word_character(ch: u8) -> bool {
    is_ascii_letter(ch) || ch.is_ascii_digit() || ch == b'_'
}

#[inline(always)]
fn is_identifier_start(ch: u8) -> bool {
    ch == b'$' || ch == b'_' || is_ascii_letter(ch)
}

#[inline(always)]
fn is_identifier_part(ch: u8) -> bool {
    ch == b'$' || is_word_character(ch)
}

fn is_line_break(ch: u8) -> bool {
    ch == b'\n' || ch == b'\r'
}

fn is_octal_digit(ch: u8) -> bool {
    ch >= b'0' && ch <= b'7'
}

const UTF8_CHAR_LEN_MAX: u32 = 6;

impl<'cx, 'p> ParserState<'cx, 'p> {
    fn ch(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    fn ch_unchecked(&self) -> u8 {
        debug_assert!(self.pos < self.end());
        unsafe { *self.input.get_unchecked(self.pos) }
    }

    fn next_ch(&self) -> Option<u8> {
        self.input.get(self.pos + 1).copied()
    }

    fn next_next_ch(&self) -> Option<u8> {
        self.input.get(self.pos + 2).copied()
    }

    fn next_next_next_ch(&self) -> Option<u8> {
        self.input.get(self.pos + 3).copied()
    }

    pub(super) fn new_span(&self, lo: usize, hi: usize) -> Span {
        Span {
            lo: lo as u32,
            hi: hi as u32,
            module: self.module_id,
        }
    }

    fn scan_number_fragment(&mut self) -> Vec<u8> {
        let start = self.pos;
        let mut allow_separator = false;
        let mut is_previous_token_separator = false;
        while let Some(ch) = self.ch() {
            if ch == b'_' {
                if allow_separator {
                    allow_separator = false;
                    is_previous_token_separator = true;
                } else {
                    todo!()
                }
                todo!()
            }

            if ch.is_ascii_digit() {
                allow_separator = true;
                is_previous_token_separator = false;
                self.pos += 1;
                continue;
            }
            break;
        }
        if self.input[self.pos - 1] == b'_' {
            todo!()
        }
        self.input[start..self.pos].to_vec()
    }

    fn scan_digits(&mut self) -> (Vec<u8>, bool) {
        let start = self.pos;
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
        let fragment = self.input[start..self.pos].to_vec();
        (fragment, is_octal)
    }

    fn scan_number(&mut self) -> Token {
        let start = self.pos;
        let fragment = if self.input[self.pos] == b'0' {
            self.pos += 1;
            if self.ch() == Some(b'_') {
                todo!()
            } else {
                let (fragment, is_oct) = self.scan_digits();
                if !is_oct {
                    todo!()
                } else if fragment.is_empty() {
                    vec![b'0']
                } else {
                    let help_lit = format!("0o{}", unsafe {
                        String::from_utf8_unchecked(fragment.clone())
                    });
                    let error = super::errors::OctalLiteralsAreNotAllowed {
                        span: self.new_span(start, self.pos),
                        help_lit,
                    };
                    self.push_error(Box::new(error));
                    fragment
                }
            }
        } else {
            self.scan_number_fragment()
        };
        if self.ch() == Some(b'.') {
            todo!()
        }
        if self.ch() == Some(b'e') || self.ch() == Some(b'E') {
            todo!()
        }
        if self.ch() == Some(b'n') {
            todo!()
        }
        let end = self.pos;
        let num = unsafe { String::from_utf8_unchecked(fragment) }
            .parse::<f64>()
            .unwrap();
        self.token_value = Some(TokenValue::Number { value: num });
        Token::new(TokenKind::Number, self.new_span(start, end))
    }

    // From `quickjs/cutils.c/unicode_from_utf8`
    #[cold]
    fn scan_unicode_from_utf8(&mut self, max_len: u32) -> PResult<()> {
        const UTF8_MIN_CODE: [u32; 5] = [0x80, 0x800, 0x10000, 0x00200000, 0x04000000];
        const UTF8_FIRST_CODE_MASK: [u32; 5] = [0x1f, 0xf, 0x7, 0x3, 0x1];

        let mut ch = self.ch_unchecked() as u32;
        let mut offset = 1;
        assert!(ch >= 0x80);
        let l = match ch {
            0xc0 | 0xc1 | 0xc2 | 0xc3 | 0xc4 | 0xc5 | 0xc6 | 0xc7 | 0xc8 | 0xc9 | 0xca | 0xcb
            | 0xcc | 0xcd | 0xce | 0xcf | 0xd0 | 0xd1 | 0xd2 | 0xd3 | 0xd4 | 0xd5 | 0xd6 | 0xd7
            | 0xd8 | 0xd9 | 0xda | 0xdb | 0xdc | 0xdd | 0xde | 0xdf => 1,
            0xe0 | 0xe1 | 0xe2 | 0xe3 | 0xe4 | 0xe5 | 0xe6 | 0xe7 | 0xe8 | 0xe9 | 0xea | 0xeb
            | 0xec | 0xed | 0xee | 0xef => 2,
            0xf0 | 0xf1 | 0xf2 | 0xf3 | 0xf4 | 0xf5 | 0xf6 | 0xf7 => 3,
            0xf8 | 0xf9 | 0xfa | 0xfb => 4,
            0xfc | 0xfd => 5,
            _ => return Err(()),
        };
        if l > (max_len - 1) {
            return Err(());
        }
        ch &= UTF8_FIRST_CODE_MASK[(l - 1) as usize];
        for _ in 0..l {
            let b = self.input[self.pos + offset] as u32;
            offset += 1;
            if b < 0x80 || b >= 0xc0 {
                return Err(());
            }
            ch = (ch << 6) | (b & 0x3f);
        }
        if ch < UTF8_MIN_CODE[(l - 1) as usize] {
            return Err(());
        }
        self.pos += offset;
        Ok(())
    }

    fn scan_identifier(&mut self, ch: u8) -> PResult<Token> {
        let start = self.pos;
        let mut first = true;
        loop {
            if first {
                if is_identifier_start(ch) {
                    self.pos += 1;
                } else {
                    assert!(ch >= 128);
                    self.scan_unicode_from_utf8(UTF8_CHAR_LEN_MAX)?;
                }
                first = false;
            } else if self.pos == self.end() {
                break;
            } else if is_identifier_part(self.ch_unchecked()) {
                self.pos += 1
            } else if self.ch_unchecked() < 128 {
                break;
            } else {
                self.scan_unicode_from_utf8(UTF8_CHAR_LEN_MAX)?
            }
        }
        let raw = &self.input[start..self.pos];
        let id = AtomId::from_bytes(raw);
        if raw.len() >= 2 && raw.len() <= 12 {
            if let Some(idx) = KEYWORDS.iter().position(|(_, kw)| (*kw == id)) {
                // keyword
                let kind = keyword_idx_to_token(idx);
                let span = self.new_span(start, self.pos);
                self.token_value = Some(TokenValue::Ident { value: id });
                return Ok(Token::new(kind, span));
            }
        }
        self.atoms
            .lock()
            .unwrap()
            .insert_if_not_exist(id, || unsafe {
                Cow::Owned(String::from_utf8_unchecked(raw.to_vec()))
            });
        self.token_value = Some(TokenValue::Ident { value: id });
        Ok(Token::new(TokenKind::Ident, self.new_span(start, self.pos)))
    }

    pub(super) fn next_token(&mut self) {
        self.full_start_pos = self.pos;
        let start = self.pos;
        self.token_flags = TokenFlags::empty();
        let mut token_start;
        loop {
            token_start = self.pos;
            if self.pos == self.end() {
                self.token = Token::new(TokenKind::EOF, self.new_span(start, start));
                return;
            }
            let ch = self.ch_unchecked();
            if self.pos == 0 && ch == b'#' {
                // TODO: Handle shebang
            }
            let token = match ch {
                b'/' => {
                    if self.next_ch() == Some(b'/') {
                        // `//`
                        self.pos += 2;
                        while self.pos < self.end() && !is_line_break(self.ch_unchecked()) {
                            self.pos += 1;
                        }
                        // TODO: add comment
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
                        continue;
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(TokenKind::SlashEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Slash, self.new_span(start, self.pos))
                    }
                }
                b'%' => {
                    if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(TokenKind::PercentEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Percent, self.new_span(start, self.pos))
                    }
                }
                b'=' => {
                    // todo: ==, ===
                    if self.next_ch() == Some(b'>') {
                        self.pos += 2;
                        Token::new(TokenKind::EqGreater, self.new_span(start, self.pos))
                    } else if self.next_ch() == Some(b'=') {
                        if self.next_next_ch() == Some(b'=') {
                            self.pos += 3;
                            Token::new(TokenKind::EqEqEq, self.new_span(start, self.pos))
                        } else {
                            self.pos += 2;
                            Token::new(TokenKind::EqEq, self.new_span(start, self.pos))
                        }
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Eq, self.new_span(start, self.pos))
                    }
                }
                b'+' => {
                    if self.next_ch() == Some(b'+') {
                        // ++
                        self.pos += 2;
                        Token::new(TokenKind::PlusPlus, self.new_span(start, self.pos))
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(TokenKind::PlusEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Plus, self.new_span(start, self.pos))
                    }
                }
                b'-' => {
                    if self.next_ch() == Some(b'-') {
                        // --
                        self.pos += 2;
                        Token::new(TokenKind::MinusMinus, self.new_span(start, self.pos))
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(TokenKind::MinusEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Minus, self.new_span(start, self.pos))
                    }
                }
                b'*' => {
                    if self.next_ch() == Some(b'*') {
                        // **
                        self.pos += 2;
                        todo!()
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(TokenKind::AsteriskEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Asterisk, self.new_span(start, self.pos))
                    }
                }
                b'|' => {
                    if self.next_ch() == Some(b'|') {
                        self.pos += 2;
                        Token::new(TokenKind::PipePipe, self.new_span(start, self.pos))
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(TokenKind::PipeEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Pipe, self.new_span(start, self.pos))
                    }
                }
                b'?' => {
                    self.pos += 1;
                    Token::new(TokenKind::Question, self.new_span(start, self.pos))
                }
                b'&' => {
                    if self.next_ch() == Some(b'&') {
                        // &&
                        self.pos += 2;
                        Token::new(TokenKind::AmpAmp, self.new_span(start, self.pos))
                    } else if self.next_ch() == Some(b'=') {
                        // &=
                        self.pos += 2;
                        Token::new(TokenKind::AmpEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Amp, self.new_span(start, self.pos))
                    }
                }
                b'<' => {
                    if self.next_ch() == Some(b'<') {
                        if self.next_next_ch() == Some(b'=') {
                            // <<=
                            self.pos += 3;
                            Token::new(TokenKind::LessLessEq, self.new_span(start, self.pos))
                        } else {
                            // <<
                            self.pos += 2;
                            Token::new(TokenKind::LessLess, self.new_span(start, self.pos))
                        }
                    } else if self.next_ch() == Some(b'=') {
                        // <=
                        self.pos += 2;
                        Token::new(TokenKind::LessEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Less, self.new_span(start, self.pos))
                    }
                }
                b'>' => {
                    if self.next_ch() == Some(b'>') {
                        if self.next_next_ch() == Some(b'>') {
                            if self.next_next_next_ch() == Some(b'=') {
                                // >>>=
                                self.pos += 4;
                                Token::new(
                                    TokenKind::GreatGreatGreatEq,
                                    self.new_span(start, self.pos),
                                )
                            } else {
                                // >>>
                                self.pos += 3;
                                Token::new(
                                    TokenKind::GreatGreatGreat,
                                    self.new_span(start, self.pos),
                                )
                            }
                        } else if self.next_next_ch() == Some(b'=') {
                            // >>=
                            self.pos += 3;
                            Token::new(TokenKind::GreatGreatEq, self.new_span(start, self.pos))
                        } else {
                            // >>
                            self.pos += 2;
                            Token::new(TokenKind::GreatGreat, self.new_span(start, self.pos))
                        }
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 1;
                        Token::new(TokenKind::GreatEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Great, self.new_span(start, self.pos))
                    }
                }
                b'^' => {
                    if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(TokenKind::CaretEq, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Caret, self.new_span(start, self.pos))
                    }
                }
                b'.' => {
                    if self.next_ch() == Some(b'.') && self.next_next_ch() == Some(b'.') {
                        self.pos += 3;
                        Token::new(TokenKind::DotDotDot, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Dot, self.new_span(start, self.pos))
                    }
                }
                b',' | b';' | b':' | b'[' | b']' | b'(' | b')' | b'{' | b'}' => {
                    self.pos += 1;
                    let kind = unsafe { std::mem::transmute::<u8, TokenKind>(ch) };
                    Token::new(kind, self.new_span(start, self.pos))
                }
                b'\'' | b'"' => {
                    let (offset, v) = self.scan_string(ch);
                    self.pos += offset;
                    let atom = self.atoms.lock().unwrap().insert_by_vec(v);
                    self.token_value = Some(TokenValue::Ident { value: atom });
                    Token::new(TokenKind::String, self.new_span(start, self.pos))
                }
                b'`' => self.scan_temp_string(),
                b'0' => self.scan_number(),
                b'1'..=b'9' => self.scan_number(),
                _ if ch.is_ascii_whitespace() => {
                    if ch == b'\n' {
                        self.token_flags.insert(TokenFlags::PRECEDING_LINE_BREAK);
                    }
                    self.pos += 1;
                    continue;
                }
                _ => self.scan_identifier(ch).unwrap(),
            };
            self.token = token;
            break;
        }
    }

    fn scan_temp_string(&mut self) -> Token {
        let start = self.pos;
        self.pos += 1;
        let mut v = Vec::with_capacity(32);
        loop {
            if self.pos == self.end() {
                break;
            } else if self.ch_unchecked() == b'`' {
                self.pos += 1;
                let atom = self.atoms.lock().unwrap().insert_by_vec(v);
                self.token_value = Some(TokenValue::Ident { value: atom });
                break;
            } else if self.ch_unchecked() == b'$' && self.next_ch() == Some(b'{') {
                todo!()
            } else {
                v.push(self.ch_unchecked());
                self.pos += 1;
            }
        }
        Token::new(
            TokenKind::NoSubstitutionTemplate,
            self.new_span(start, self.pos),
        )
    }

    fn scan_string(&self, quote: u8) -> (usize, Vec<u8>) {
        let start = self.pos;
        let mut offset = 1;
        let mut v = Vec::with_capacity(32);
        loop {
            let idx = start + offset;
            if idx >= self.end() {
                break;
            }
            let ch = self.input[idx];
            offset += 1;
            if ch == quote {
                break;
            }
            v.push(ch);
        }
        (offset, v)
    }

    pub(super) fn re_scan_greater(&mut self) -> TokenKind {
        self.token.kind
    }

    pub(super) fn re_scan_less(&mut self) -> TokenKind {
        self.token.kind
    }
}
