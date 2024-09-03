use crate::span::Span;

use super::ast::BinOp;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn is_keyword(&self) -> bool {
        false
    }

    pub fn start(&self) -> u32 {
        assert!(self.span.lo != u32::MAX);
        self.span.lo
    }

    pub fn end(&self) -> u32 {
        assert!(self.span.lo != u32::MAX);
        self.span.hi
    }

}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    // keyword
    False,
    True,
    // =====
    Plus,
    // =====
    Number,
    Unknown,
    EOF,
}

impl TokenKind {
    pub fn prec(self) -> BinPrec {
        match self {
            TokenKind::Plus => BinPrec::Additive,
            _ => BinPrec::Invalid
        }
    }

    pub fn into_binop(self) -> BinOp {
        match self {
            TokenKind::Plus => BinOp::Add ,
            _ => unreachable!()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinPrec {
    Invalid,
    Lowest,
    /// `+` or `-`
    Additive,
    Highest,
}

bitflags::bitflags! {
    pub struct TokenFlags: u16 {
        const NONE = 0;
        const PRECEDING_LINE_BREAK  = 1 << 0;
        /// `123_456`
        const CONTAINS_SEPARATOR    = 1 << 9;
        const NUMERIC_LITERAL_FLAGS = TokenFlags::CONTAINS_SEPARATOR.bits() | TokenFlags::PRECEDING_LINE_BREAK.bits();
    }
}
