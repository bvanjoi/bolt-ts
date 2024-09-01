use super::token::{Token, TokenFlags, TokenKind};
use super::TsParserState;

use crate::span::Span;

impl<'cx, 'a, 'p> TsParserState<'cx, 'p> {
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
        Token::new(TokenKind::Number(num), Span::from((start, end)))
    }

    pub(super) fn next_token(&mut self) {
        if self.token.is_keyword() {}
        let start = self.pos;
        let flags = TokenFlags::NONE;
        let mut token_start;
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
            b'1'..=b'9' => self.scan_number(),
            _ => todo!(),
        };
        self.token = token;
    }
}
