use super::keyword::KEYWORDS;
use super::token::{Token, TokenFlags, TokenKind};
use super::ParserState;

use crate::atoms::AtomId;
use crate::span::Span;

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

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    fn ch(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
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
        if self.input[self.pos] == b'0' {
            todo!()
        }
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
        self.token_number_value = Some(num);
        Token::new(TokenKind::Number, Span::from((start, end)))
    }

    fn scan_identifier(&mut self, ch: u8) -> Token {
        let start = self.pos;
        if is_identifier_start(ch) {
            self.pos += 1;
            loop {
                if self.pos == self.end() {
                    break;
                } else if !is_identifier_part(ch) {
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
                    let span = Span::from((start, self.pos));
                    return Token::new(kind, span);
                }
            }
        }
        // Token;
        Token::new(TokenKind::EOF, Span::from((0, 0)))
    }

    pub(super) fn next_token(&mut self) {
        if self.token.is_keyword() {}
        let start = self.pos;
        let flags = TokenFlags::NONE;
        let mut token_start;
        loop {
            token_start = self.pos;
            if self.pos == self.input.len() {
                self.token = Token::new(TokenKind::EOF, Span::from((start, start)));
                return;
            }
            let ch = self.input[self.pos];
            if self.pos == 0 && ch == b'#' {
                // TODO: Handle shebang
            }
            let token = match ch {
                b'0' => todo!(),
                b'+' => {
                    if self.input.get(self.pos + 1).copied() == Some(b'+') {
                        // ++
                        self.pos += 2;
                        todo!()
                    } else {
                        self.pos += 1;
                        Token::new(TokenKind::Plus, Span::from((start, start + 1)))
                    }
                }
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
}
