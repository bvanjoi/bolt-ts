use std::borrow::Cow;

use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;

use super::token::{keyword_idx_to_token, Token, TokenFlags, TokenKind};
use super::{PResult, ParserState, TokenValue};

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

#[inline(always)]
fn is_line_break(ch: u8) -> bool {
    ch == b'\n' || ch == b'\r'
}

#[inline(always)]
fn is_octal_digit(ch: u8) -> bool {
    (b'0'..=b'7').contains(&ch)
}

const UTF8_CHAR_LEN_MAX: u32 = 6;

impl ParserState<'_, '_> {
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

    pub(super) fn new_span(&self, lo: u32) -> Span {
        Span {
            lo,
            hi: self.full_start_pos as u32,
            module: self.module_id,
        }
    }

    fn scan_number_fragment(&mut self) -> Vec<u8> {
        let start = self.pos;
        let mut allow_separator = false;
        // let mut is_previous_token_separator = false;
        while let Some(ch) = self.ch() {
            if ch == b'_' {
                if allow_separator {
                    allow_separator = false;
                    // is_previous_token_separator = true;
                } else {
                    todo!()
                }
                todo!()
            }

            if ch.is_ascii_digit() {
                allow_separator = true;
                // is_previous_token_separator = false;
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
                        span: Span::new(start as u32, self.pos as u32, self.module_id),
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
        let kind = if self.ch() == Some(b'n') {
            self.pos += 1;
            let value = self.atoms.lock().unwrap().insert_by_vec(fragment);
            self.token_value = Some(TokenValue::Ident { value });
            TokenKind::BigInt
        } else {
            let num = unsafe { String::from_utf8_unchecked(fragment) }
                .parse::<f64>()
                .unwrap();
            self.token_value = Some(TokenValue::Number { value: num });
            TokenKind::Number
        };
        let end = self.pos;
        Token::new(kind, Span::new(start as u32, end as u32, self.module_id))
    }

    // From `quickjs/cutils.c/unicode_from_utf8`
    #[cold]
    fn scan_unicode_from_utf8(&mut self, max_len: u32) -> PResult<()> {
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
            _ => return Err(()),
        };
        if l > (max_len - 1) {
            return Err(());
        }
        let idx = (l - 1) as usize;
        ch &= UTF8_FIRST_CODE_MASK[idx];
        for _ in 0..l {
            let b = self.input[self.pos + offset] as u32;
            if !(0x80..0xc0).contains(&b) {
                return Err(());
            }
            offset += 1;
            ch = (ch << 6) | (b & 0x3f);
        }
        if ch < UTF8_MIN_CODE[idx] {
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
                    assert!(ch >= 128, "invalid char: {ch}");
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
                let span = Span::new(start as u32, self.pos as u32, self.module_id);
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
        Ok(Token::new(
            TokenKind::Ident,
            Span::new(start as u32, self.pos as u32, self.module_id),
        ))
    }

    pub(super) fn next_token(&mut self) {
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
                b'~' => {
                    self.pos += 1;
                    Token::new(
                        TokenKind::Tilde,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'*' => {
                    if self.next_ch() == Some(b'*') {
                        // **
                        self.pos += 2;
                        todo!()
                    } else if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::AsteriskEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
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
                    self.pos += 1;
                    Token::new(
                        TokenKind::Question,
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
                b'>' => {
                    // `>>`, `>=`, `>>>`, `>>=`, `>>>=` will be handled in `re_scan_greater`
                    self.pos += 1;
                    Token::new(
                        TokenKind::Great,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'^' => {
                    if self.next_ch() == Some(b'=') {
                        self.pos += 2;
                        Token::new(
                            TokenKind::CaretEq,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    } else {
                        self.pos += 1;
                        Token::new(
                            TokenKind::Caret,
                            Span::new(start as u32, self.pos as u32, self.module_id),
                        )
                    }
                }
                b'.' if self.next_ch() == Some(b'.') && self.next_next_ch() == Some(b'.') => {
                    self.pos += 3;
                    Token::new(
                        TokenKind::DotDotDot,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
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
                b',' | b';' | b':' | b'[' | b']' | b'(' | b')' | b'{' | b'}' | b'!' | b'.' => {
                    self.pos += 1;
                    let kind = unsafe { std::mem::transmute::<u8, TokenKind>(ch) };
                    Token::new(
                        kind,
                        Span::new(start as u32, self.pos as u32, self.module_id),
                    )
                }
                b'\'' | b'"' => {
                    let (offset, v, key_value) = self.scan_string(ch);
                    self.pos += offset;

                    let len = v.len();
                    let atom = self.atoms.lock().unwrap().insert_by_vec(v);
                    if len != key_value.len() {
                        let key_atom = self.atoms.lock().unwrap().insert_by_vec(key_value);
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
            Span::new(start as u32, self.pos as u32, self.module_id),
        )
    }

    fn scan_string(&self, quote: u8) -> (usize, Vec<u8>, Vec<u8>) {
        let start = self.pos;
        let mut offset = 1;
        let mut v = Vec::with_capacity(32);
        let mut prev = 0;
        let mut key = Vec::with_capacity(32);
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
            if !(ch == b'\n' && prev == b'\\') {
                key.push(ch);
            } else {
                key.pop();
            }
            v.push(ch);
            prev = ch;
        }
        (offset, v, key)
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
        self.token.kind
    }
}
