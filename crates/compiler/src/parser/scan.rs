use std::borrow::Cow;

use rts_span::Span;

use super::token::{Token, TokenFlags, TokenKind};
use super::{ParserState, TokenValue};

use crate::atoms::AtomId;
use crate::keyword::KEYWORDS;

#[inline(always)]
pub fn is_ascii_letter(ch: u8) -> bool {
    ch.is_ascii_alphabetic()
}

pub fn is_word_character(ch: u8) -> bool {
    is_ascii_letter(ch) || ch.is_ascii_digit() || ch == b'_'
}

#[inline(always)]
pub fn is_identifier_start(ch: u8) -> bool {
    is_ascii_letter(ch) || ch == b'$' || ch == b'_'
}

pub fn is_identifier_part(ch: u8) -> bool {
    is_word_character(ch) || ch == b'$'
}

fn is_line_break(ch: u8) -> bool {
    ch == b'\n' || ch == b'\r'
}

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
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

    fn scan_number(&mut self) -> Token {
        let start = self.pos;
        // if self.input[self.pos] == b'0' {
        //     todo!()
        // }
        let fragment = self.scan_number_fragment();
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

    fn scan_identifier(&mut self, ch: u8) -> Token {
        let start = self.pos;
        if is_identifier_start(ch) {
            self.pos += 1;
            loop {
                if self.pos == self.end() {
                    break;
                } else if !is_identifier_part(self.ch_unchecked()) {
                    break;
                } else {
                    self.pos += 1;
                }
            }
            let raw = &self.input[start..self.pos];
            let id = AtomId::from_bytes(raw);
            if raw.len() >= 2 && raw.len() <= 12 {
                if let Some(idx) = KEYWORDS
                    .iter()
                    .enumerate()
                    .find_map(|(idx, kw)| (kw.1 == id).then_some(idx))
                {
                    // keyword
                    let kind = unsafe { std::mem::transmute::<u8, TokenKind>(idx as u8) };
                    let span = self.new_span(start, self.pos);
                    self.token_value = Some(TokenValue::Ident { value: id });
                    return Token::new(kind, span);
                }
            }
            self.p.atoms.insert_if_not_exist(id, || unsafe {
                Cow::Owned(String::from_utf8_unchecked(raw.to_vec()))
            });
            self.token_value = Some(TokenValue::Ident { value: id });
            Token::new(TokenKind::Ident, self.new_span(start, self.pos))
        } else {
            unreachable!("pos: {} ch: {}", self.pos, ch as char);
        }
    }

    pub(super) fn next_token(&mut self) {
        let start = self.pos;
        let flags = TokenFlags::empty();
        let mut token_start;
        loop {
            token_start = self.pos;
            if self.pos == self.end() {
                self.token = Token::new(TokenKind::EOF, self.new_span(start, start));
                return;
            }
            let ch = self.input[self.pos];
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
                    } else {
                        self.pos += 1;
                        continue;
                    }
                }
                b'=' => {
                    // todo: ==, ===
                    if self.next_ch() == Some(b'>') {
                        self.pos += 2;
                        Token::new(TokenKind::EqGreater, self.new_span(start, self.pos))
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Eq, self.new_span(start, self.pos))
                    }
                }
                b'+' => {
                    if self.next_ch() == Some(b'+') {
                        // ++
                        self.pos += 2;
                        todo!()
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Plus, self.new_span(start, self.pos))
                    }
                }
                b'|' => {
                    if self.next_ch() == Some(b'|') {
                        self.pos += 2;
                        Token::new(TokenKind::PipePipe, self.new_span(start, self.pos))
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
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Amp, self.new_span(start, self.pos))
                    }
                }
                b'<' => {
                    self.pos += 1;
                    Token::new(TokenKind::Great, self.new_span(start, self.pos))
                }
                b'>' => {
                    self.pos += 1;
                    Token::new(TokenKind::Less, self.new_span(start, self.pos))
                }
                b'.' => {
                    if self.next_ch() == Some(b'.') && self.next_next_ch() == Some(b'.') {
                        self.pos += 3;
                        Token::new(TokenKind::DotDotDot, self.new_span(start, self.pos))
                    } else {
                        todo!()
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
                    let atom = self.p.atoms.insert_by_vec(v);
                    self.token_value = Some(TokenValue::Ident { value: atom });
                    Token::new(TokenKind::String, self.new_span(start, self.pos))
                }
                b'`' => self.scan_temp_string(),
                b'0' => self.scan_number(),
                b'1'..=b'9' => self.scan_number(),
                _ if ch.is_ascii_whitespace() => {
                    self.pos += 1;
                    continue;
                }
                _ => self.scan_identifier(ch),
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
                let atom = self.p.atoms.insert_by_vec(v);
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
        while start + offset < self.end() {
            let ch = self.input[start + offset];
            offset += 1;
            if ch == quote {
                break;
            }
            v.push(ch);
        }
        (offset, v)
    }
}
