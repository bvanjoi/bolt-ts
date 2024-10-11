use rts_span::Span;

use crate::ast::BinOpKind;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // keyword
    Null,
    False,
    True,
    Var,
    Let,
    Const,
    Function,
    Return,
    If,
    Else,
    // =====
    EOF,
    Number,
    String,
    Ident,
    Unknown,
    // =====
    /// `+`
    Plus = 0x2B,
    /// `=`
    Eq = 0x3D,
    /// `|`
    Pipe = 0x7C,
    /// `?`
    Question = 0x3F,
    /// `,`
    Comma = 0x2C,
    /// `;`
    Semi = 0x3B,
    /// `:`
    Colon = 0x3A,
    /// `{`
    LBrace = 0x7B,
    /// `}`
    RBrace = 0x7D,
    /// `[`
    LBracket = 0x5B,
    /// `]`
    RBracket = 0x5D,
    /// `(`
    LParen = 0x28,
    /// `)`
    RParen = 0x29,
    // ======
    /// `=>`
    EqGreater
}

impl TokenKind {
    pub fn prec(self) -> BinPrec {
        match self {
            TokenKind::Pipe => BinPrec::BitwiseOR,
            TokenKind::Plus => BinPrec::Additive,
            _ => BinPrec::Invalid,
        }
    }

    pub fn into_binop(self) -> BinOpKind {
        match self {
            TokenKind::Plus => BinOpKind::Add,
            TokenKind::Pipe => BinOpKind::Pipe,
            _ => unreachable!(),
        }
    }

    pub fn is_start_of_stmt(self) -> bool {
        use TokenKind::*;
        matches!(self, Semi | Var | Let | Function | If | Return)
    }

    pub fn is_start_of_left_hand_side_expr(self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Number | True | False | String | Ident | Null | LBrace | LBracket | LParen
        )
    }

    pub fn is_start_of_expr(self) -> bool {
        self.is_start_of_left_hand_side_expr()
    }

    pub fn is_binding_ident(self) -> bool {
        matches!(self, TokenKind::Ident)
    }

    pub fn is_binding_ident_or_private_ident_or_pat(self) -> bool {
        self.is_binding_ident()
    }

    pub fn is_ident_or_keyword(self) -> bool {
        matches!(self, TokenKind::Ident)
    }

    pub fn is_lit_prop_name(self) -> bool {
        use TokenKind::*;
        self.is_ident_or_keyword() || matches!(self, String | Number)
    }

    pub fn is_start_of_param(self) -> bool {
        self.is_binding_ident_or_private_ident_or_pat()
    }

    pub fn is_start_of_type(self) -> bool {
        use TokenKind::*;
        matches!(self, LBrace | LBracket)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinPrec {
    Invalid,
    Lowest,
    /// `|`
    BitwiseOR,
    /// `+`, `-`
    Additive,
    Highest,
}

bitflags::bitflags! {
    pub struct TokenFlags: u16 {
        const NONE = 0;
        const PRECEDING_LINE_BREAK  = 1 << 0;
        /// `123_456`
        const CONTAINS_SEPARATOR    = 1 << 9;
        const NUMERIC_LITERAL_FLAGS = Self::CONTAINS_SEPARATOR.bits() | Self::PRECEDING_LINE_BREAK.bits();
    }
}
