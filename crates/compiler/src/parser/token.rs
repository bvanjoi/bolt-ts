use bolt_ts_span::Span;

use crate::ast::{AssignOp, BinOpKind, ModifierKind, PrefixUnaryOp};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
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

pub const KEYWORD_TOKEN_START: u8 = TokenKind::Null as u8;
pub const KEYWORD_TOKEN_END: u8 = TokenKind::Type as u8;

pub const fn keyword_idx_to_token(idx: usize) -> TokenKind {
    unsafe { std::mem::transmute::<u8, TokenKind>(idx as u8 + KEYWORD_TOKEN_START) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // =====
    EOF,
    Number,
    String,
    Ident,
    NoSubstitutionTemplate,
    // ======
    /// `=>`
    EqGreat,
    /// `...`
    DotDotDot,
    /// `&=`
    AmpEq,
    /// `&&`
    AmpAmp,
    /// `||`
    PipePipe,
    /// `|=`
    PipeEq,
    /// `==`
    EqEq,
    /// `===`
    EqEqEq,
    /// `+=`
    PlusEq,
    /// `++`
    PlusPlus,
    /// `-=`
    MinusEq,
    /// `--`
    MinusMinus,
    /// `*=`
    AsteriskEq,
    /// `/=`
    SlashEq,
    /// `%=`
    PercentEq,
    /// `<<`
    LessLess,
    /// `<=`
    LessEq,
    /// `<<=`
    LessLessEq,
    /// `>=`
    GreatEq,
    /// `>>`
    GreatGreat,
    /// `>>>`
    GreatGreatGreat,
    /// `>>=`
    GreatGreatEq,
    /// `>>>=`
    GreatGreatGreatEq,
    /// `!==`
    BangEqEq,
    /// `!=`
    BangEq,
    /// `^=`
    CaretEq,
    // =====
    /// `!`
    Excl = 0x21,
    /// `%`
    Percent = 0x25,
    /// `&`
    Amp = 0x26,
    /// `*`
    Asterisk = 0x2A,
    /// `+`
    Plus = 0x2B,
    /// `,`
    Comma = 0x2C,
    /// `-`
    Minus = 0x2D,
    /// `(`
    LParen = 0x28,
    /// `)`
    RParen = 0x29,
    /// `.`
    Dot = 0x2E,
    /// `:`
    Colon = 0x3A,
    /// `;`
    Semi = 0x3B,
    /// `<`
    Less = 0x3C,
    /// `=`
    Eq = 0x3D,
    /// `>`
    Great = 0x3E,
    /// `?`
    Question = 0x3F,
    /// `@`
    At = 0x40,
    /// `[`
    LBracket = 0x5B,
    /// `\`
    Slash = 0x5C,
    /// `]`
    RBracket = 0x5D,
    /// `^`
    Caret = 0x5E,
    /// `|`
    Pipe = 0x7C,
    /// `{`
    LBrace = 0x7B,
    /// `}`
    RBrace = 0x7D,
    // =====
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
    Class,
    Extends,
    New,
    Async,
    This,
    Static,
    Constructor,
    Super,
    Get,
    Set,
    Import,
    Export,
    Default,
    Throw,
    Try,
    Catch,
    Finally,
    Debugger,
    Typeof,
    In,
    // ts keyword
    Implements,
    Interface,
    Abstract,
    Public,
    Private,
    As,
    Is,
    Declare,
    Module,
    Namespace,
    Enum,
    Readonly,
    Type,
}

impl From<TokenKind> for BinOpKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => BinOpKind::Add,
            TokenKind::Pipe => BinOpKind::Pipe,
            TokenKind::PipePipe => BinOpKind::PipePipe,
            TokenKind::EqEq => BinOpKind::EqEq,
            TokenKind::EqEqEq => BinOpKind::EqEqEq,
            TokenKind::Minus => BinOpKind::Sub,
            TokenKind::Asterisk => BinOpKind::Mul,
            TokenKind::AsteriskEq => BinOpKind::Mul,
            TokenKind::Slash => BinOpKind::Div,
            TokenKind::SlashEq => BinOpKind::Div,
            TokenKind::Amp => BinOpKind::BitAnd,
            TokenKind::AmpAmp => BinOpKind::LogicalAnd,
            TokenKind::Less => BinOpKind::Less,
            TokenKind::LessEq => BinOpKind::LessEq,
            TokenKind::LessLess => BinOpKind::Shl,
            TokenKind::Great => BinOpKind::Great,
            TokenKind::GreatEq => BinOpKind::GreatEq,
            TokenKind::GreatGreat => BinOpKind::Shr,
            TokenKind::GreatGreatGreat => BinOpKind::UShr,
            _ => {
                unreachable!("{:#?}", value)
            }
        }
    }
}

impl From<TokenKind> for PrefixUnaryOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => PrefixUnaryOp::Plus,
            TokenKind::Minus => PrefixUnaryOp::Minus,
            TokenKind::PlusPlus => PrefixUnaryOp::PlusPlus,
            TokenKind::MinusMinus => PrefixUnaryOp::MinusMinus,
            _ => {
                unreachable!("{:#?}", value)
            }
        }
    }
}

impl TryFrom<TokenKind> for ModifierKind {
    type Error = ();
    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Public => Ok(ModifierKind::Public),
            TokenKind::Abstract => Ok(ModifierKind::Abstract),
            TokenKind::Static => Ok(ModifierKind::Static),
            TokenKind::Declare => Ok(ModifierKind::Declare),
            TokenKind::Private => Ok(ModifierKind::Private),
            TokenKind::Export => Ok(ModifierKind::Export),
            TokenKind::Readonly => Ok(ModifierKind::Readonly),
            _ => Err(()),
        }
    }
}

impl From<TokenKind> for AssignOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Eq => AssignOp::Eq,
            TokenKind::PlusEq => AssignOp::AddEq,
            TokenKind::MinusEq => AssignOp::SubEq,
            TokenKind::AsteriskEq => AssignOp::MulEq,
            TokenKind::SlashEq => AssignOp::DivEq,
            TokenKind::PercentEq => AssignOp::ModEq,
            TokenKind::AmpEq => AssignOp::BitAndEq,
            TokenKind::PipeEq => AssignOp::BitOrEq,
            TokenKind::LessLessEq => AssignOp::ShlEq,
            TokenKind::GreatGreatEq => AssignOp::ShrEq,
            TokenKind::GreatGreatGreatEq => AssignOp::UShrEq,
            TokenKind::CaretEq => AssignOp::BitXorEq,
            _ => unreachable!(),
        }
    }
}

impl TokenKind {
    pub fn prec(self) -> BinPrec {
        use TokenKind::*;
        match self {
            Pipe => BinPrec::BitwiseOR,
            Less | LessEq | Great | GreatEq => BinPrec::Relational,
            LessLess | GreatGreat | GreatGreatGreat => BinPrec::Shift,
            Plus | Minus => BinPrec::Additive,
            PipePipe => BinPrec::LogicalOr,
            AmpAmp => BinPrec::LogicalAnd,
            EqEq | EqEqEq => BinPrec::Eq,
            Asterisk | Slash | Percent => BinPrec::Multiplicative,
            _ => BinPrec::Invalid,
        }
    }

    pub(super) fn is_ident(&self) -> bool {
        matches!(self, TokenKind::Ident | TokenKind::Abstract)
    }

    pub fn is_start_of_left_hand_side_expr(self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Null | True
                | False
                | Number
                | String
                | LBrace
                | LBracket
                | LParen
                | This
                | Function
                | Class
                | New
                | Slash
                | SlashEq
                | Ident
        ) || self.is_ident()
    }

    fn is_ts_keyword(self) -> bool {
        let u = self as u8;
        u <= KEYWORD_TOKEN_END && u >= (TokenKind::Implements as u8)
    }

    pub fn is_binding_ident(self) -> bool {
        matches!(self, TokenKind::Ident) || self.is_ts_keyword()
    }

    pub fn is_binding_ident_or_private_ident_or_pat(self) -> bool {
        self.is_binding_ident()
    }

    pub fn is_keyword(self) -> bool {
        let u = self as u8;
        u >= KEYWORD_TOKEN_START && u <= KEYWORD_TOKEN_END
    }

    pub fn is_ident_or_keyword(self) -> bool {
        matches!(self, TokenKind::Ident) || self.is_keyword()
    }

    pub fn is_lit_prop_name(self) -> bool {
        use TokenKind::*;
        self.is_ident_or_keyword() || matches!(self, String | Number)
    }

    pub fn is_start_of_param(self) -> bool {
        matches!(self, TokenKind::DotDotDot) || self.is_binding_ident_or_private_ident_or_pat()
    }

    pub fn is_heritage_clause(&self) -> bool {
        matches!(self, TokenKind::Extends | TokenKind::Implements)
    }

    pub fn is_assignment(self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Eq | PlusEq
                | MinusEq
                | AsteriskEq
                | SlashEq
                | PercentEq
                | AmpEq
                | PipeEq
                | CaretEq
                | LessLessEq
                | GreatGreatEq
                | GreatGreatGreatEq
        )
    }

    pub fn is_modifier_kind(self) -> bool {
        TryInto::<ModifierKind>::try_into(self).is_ok()
    }

    pub fn is_accessibility_modifier(self) -> bool {
        use TokenKind::*;
        matches!(self, Public)
    }

    pub fn is_param_prop_modifier(self) -> bool {
        // TODO: readonly | override
        self.is_accessibility_modifier()
    }

    pub fn is_class_ele_modifier(self) -> bool {
        self.is_param_prop_modifier()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinPrec {
    Invalid,
    Lowest,
    /// `||`
    LogicalOr,
    /// `&&`
    LogicalAnd,
    /// `|`
    BitwiseOR,
    /// `==`, `===`
    Eq,
    /// `<=`, `>=`, `<`, `>`
    Relational,
    /// `<<`, `>>`, `>>=`
    Shift,
    /// `+`, `-`
    Additive,
    /// `*`, `/`, `%`   
    Multiplicative,
    Highest,
}

bitflags::bitflags! {
    pub struct TokenFlags: u16 {
        const PRECEDING_LINE_BREAK  = 1 << 0;
        /// `123_456`
        const CONTAINS_SEPARATOR    = 1 << 9;
        const NUMERIC_LITERAL_FLAGS = Self::CONTAINS_SEPARATOR.bits() | Self::PRECEDING_LINE_BREAK.bits();
    }
}
