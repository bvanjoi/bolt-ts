use bolt_ts_atom::Atom;
use bolt_ts_span::Span;

use crate::keyword;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub fn start(&self) -> u32 {
        debug_assert_ne!(self.span.lo(), u32::MAX);
        self.span.lo()
    }

    #[inline]
    pub fn end(&self) -> u32 {
        debug_assert_ne!(self.span.hi(), u32::MAX);
        self.span.hi()
    }
}

const KEYWORD_TOKEN_START: u8 = TokenKind::Null as u8;
const KEYWORD_TOKEN_END: u8 = TokenKind::Type as u8;

pub const fn keyword_idx_to_token(idx: usize) -> TokenKind {
    unsafe { std::mem::transmute::<u8, TokenKind>(idx as u8 + KEYWORD_TOKEN_START) }
}

pub fn atom_to_token(id: Atom) -> Option<TokenKind> {
    static KWS: std::sync::LazyLock<rustc_hash::FxHashMap<Atom, TokenKind>> =
        std::sync::LazyLock::new(|| {
            [
                (keyword::KW_NULL, TokenKind::Null),
                (keyword::KW_FALSE, TokenKind::False),
                (keyword::KW_TRUE, TokenKind::True),
                (keyword::KW_VAR, TokenKind::Var),
                (keyword::KW_LET, TokenKind::Let),
                (keyword::KW_CONST, TokenKind::Const),
                (keyword::KW_FUNCTION, TokenKind::Function),
                (keyword::KW_RETURN, TokenKind::Return),
                (keyword::KW_IF, TokenKind::If),
                (keyword::KW_ELSE, TokenKind::Else),
                (keyword::KW_CLASS, TokenKind::Class),
                (keyword::KW_EXTENDS, TokenKind::Extends),
                (keyword::KW_NEW, TokenKind::New),
                (keyword::KW_ASYNC, TokenKind::Async),
                (keyword::KW_AWAIT, TokenKind::Await),
                (keyword::KW_THIS, TokenKind::This),
                (keyword::KW_STATIC, TokenKind::Static),
                (keyword::KW_CONSTRUCTOR, TokenKind::Constructor),
                (keyword::KW_SUPER, TokenKind::Super),
                (keyword::KW_GET, TokenKind::Get),
                (keyword::KW_SET, TokenKind::Set),
                (keyword::KW_IMPORT, TokenKind::Import),
                (keyword::KW_EXPORT, TokenKind::Export),
                (keyword::KW_FROM, TokenKind::From),
                (keyword::KW_DEFAULT, TokenKind::Default),
                (keyword::KW_THROW, TokenKind::Throw),
                (keyword::KW_TRY, TokenKind::Try),
                (keyword::KW_CATCH, TokenKind::Catch),
                (keyword::KW_FINALLY, TokenKind::Finally),
                (keyword::KW_DEBUGGER, TokenKind::Debugger),
                (keyword::KW_DELETE, TokenKind::Delete),
                (keyword::KW_TYPEOF, TokenKind::Typeof),
                (keyword::KW_PACKAGE, TokenKind::Package),
                (keyword::KW_YIELD, TokenKind::Yield),
                (keyword::KW_FOR, TokenKind::For),
                (keyword::KW_OF, TokenKind::Of),
                (keyword::KW_WHILE, TokenKind::While),
                (keyword::KW_DO, TokenKind::Do),
                (keyword::KW_SWITCH, TokenKind::Switch),
                (keyword::KW_CASE, TokenKind::Case),
                (keyword::KW_BREAK, TokenKind::Break),
                (keyword::KW_CONTINUE, TokenKind::Continue),
                (keyword::KW_INSTANCEOF, TokenKind::Instanceof),
                (keyword::KW_VOID, TokenKind::Void),
                (keyword::KW_UNDEFINED, TokenKind::Undefined),
                (keyword::KW_IN, TokenKind::In),
                (keyword::KW_IMPLEMENTS, TokenKind::Implements),
                (keyword::KW_INTERFACE, TokenKind::Interface),
                (keyword::KW_ABSTRACT, TokenKind::Abstract),
                (keyword::KW_PUBLIC, TokenKind::Public),
                (keyword::KW_PROTECTED, TokenKind::Protected),
                (keyword::KW_PRIVATE, TokenKind::Private),
                (keyword::KW_AS, TokenKind::As),
                (keyword::KW_IS, TokenKind::Is),
                (keyword::KW_DECLARE, TokenKind::Declare),
                (keyword::KW_MODULE, TokenKind::Module),
                (keyword::KW_NAMESPACE, TokenKind::Namespace),
                (keyword::KW_ENUM, TokenKind::Enum),
                (keyword::KW_READONLY, TokenKind::Readonly),
                (keyword::KW_SATISFIES, TokenKind::Satisfies),
                (keyword::KW_KEYOF, TokenKind::Keyof),
                (keyword::KW_INFER, TokenKind::Infer),
                (keyword::KW_INTRINSIC, TokenKind::Intrinsic),
                (keyword::KW_UNIQUE, TokenKind::Unique),
                (keyword::KW_ASSERTS, TokenKind::Asserts),
                (keyword::KW_TYPE, TokenKind::Type),
            ]
            .into_iter()
            .collect()
        });
    KWS.get(&id).copied()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenKind {
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
    /// `**`
    AsteriskAsterisk,
    /// `**=`
    AsteriskAsteriskEq,
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
    /// `?.`
    QuestionDot,
    /// `</`
    LessSlash,
    // =====
    /// `!`
    Excl = 0x21,
    /// `$`
    Dollar = 0x24,
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
    /// `` ` ``
    Backtick = 0x60,
    /// `|`
    Pipe = 0x7C,
    /// `{`
    LBrace = 0x7B,
    /// `}`
    RBrace = 0x7D,
    /// `~`
    Tilde = 0x7E,
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
    Await,
    This,
    Static,
    Constructor,
    Super,
    Get,
    Set,
    Import,
    Export,
    From,
    Default,
    Throw,
    Try,
    Catch,
    Finally,
    Debugger,
    Delete,
    Typeof,
    Package,
    Yield,
    For,
    Of,
    While,
    Do,
    Switch,
    Case,
    Break,
    Continue,
    Instanceof,
    Void,
    Undefined,
    In,
    // ts keyword
    Implements,
    Interface,
    Abstract,
    Public,
    Protected,
    Private,
    As,
    Is,
    Declare,
    Module,
    Namespace,
    Enum,
    Readonly,
    Satisfies,
    Keyof,
    Infer,
    Intrinsic,
    Unique,
    Asserts,
    Type,
    // =====
    EOF,
    Unknown,
    /// number literal
    Number,
    /// string literal
    String,
    /// bigint literal
    BigInt,
    JSXText,
    JSXTextAllWhiteSpaces,
    Regexp,
    Ident,
    NoSubstitutionTemplate,
    TemplateHead,
    TemplateMiddle,
    TemplateTail,
}

impl TokenKind {
    pub fn as_str(&self) -> &'static str {
        use TokenKind::*;
        match self {
            EqGreat => "=>",
            DotDotDot => "...",
            AmpEq => "&=",
            AmpAmp => "&&",
            PipePipe => "||",
            PipeEq => "|=",
            EqEq => "==",
            EqEqEq => "===",
            PlusEq => "+=",
            PlusPlus => "++",
            MinusEq => "-=",
            MinusMinus => "--",
            AsteriskEq => "*=",
            SlashEq => "/=",
            PercentEq => "%=",
            LessLess => "<<",
            LessEq => "<=",
            LessLessEq => "<<=",
            GreatEq => ">=",
            GreatGreat => ">>",
            GreatGreatGreat => ">>>",
            GreatGreatEq => ">>=",
            GreatGreatGreatEq => ">>>=",
            BangEqEq => "!==",
            BangEq => "!=",
            CaretEq => "^=",
            Excl => "!",
            Percent => "%",
            Amp => "&",
            Asterisk => "*",
            Plus => "+",
            Comma => ",",
            Minus => "-",
            LParen => "(",
            RParen => ")",
            Dot => ".",
            Colon => ":",
            Semi => ";",
            Less => "<",
            Eq => "=",
            Great => ">",
            Question => "?",
            At => "@",
            LBracket => "[",
            Slash => "\\",
            RBracket => "]",
            Caret => "^",
            Pipe => "|",
            LBrace => "{",
            RBrace => "}",
            LessSlash => "</",
            Null => "null",
            False => "false",
            True => "true",
            Var => "var",
            Let => "let",
            Const => "const",
            Function => "function",
            Return => "return",
            If => "if",
            Else => "else",
            Class => "class",
            Extends => "extends",
            New => "new",
            Async => "async",
            Await => "await",
            This => "this",
            Static => "static",
            Constructor => "constructor",
            Super => "super",
            Get => "get",
            Set => "set",
            Import => "import",
            Export => "export",
            From => "from",
            Default => "default",
            Throw => "throw",
            Try => "try",
            Catch => "catch",
            Finally => "finally",
            Debugger => "debugger",
            Typeof => "typeof",
            Package => "package",
            Yield => "yield",
            For => "for",
            Of => "of",
            Break => "break",
            Continue => "continue",
            Instanceof => "instanceof",
            In => "in",
            Implements => "implements",
            Interface => "interface",
            Abstract => "abstract",
            Public => "public",
            Protected => "protected",
            Private => "private",
            As => "as",
            Is => "is",
            Declare => "declare",
            Module => "module",
            Namespace => "namespace",
            Enum => "enum",
            Readonly => "readonly",
            Satisfies => "satisfies",
            Keyof => "keyof",
            Infer => "infer",
            Intrinsic => "intrinsic",
            Type => "type",
            Unique => "unique",
            Asserts => "asserts",
            _ => unreachable!("{:#?}", self),
        }
    }
}

impl From<TokenKind> for super::BinOpKind {
    fn from(value: TokenKind) -> Self {
        use super::BinOpKind::*;
        match value {
            TokenKind::Plus => Add,
            TokenKind::Pipe => BitOr,
            TokenKind::PipePipe => PipePipe,
            TokenKind::EqEq => EqEq,
            TokenKind::EqEqEq => EqEqEq,
            TokenKind::Minus => Sub,
            TokenKind::Asterisk => Mul,
            TokenKind::AsteriskEq => Mul,
            TokenKind::Slash => Div,
            TokenKind::SlashEq => Div,
            TokenKind::Percent => Mod,
            TokenKind::Amp => BitAnd,
            TokenKind::AmpAmp => LogicalAnd,
            TokenKind::Less => Less,
            TokenKind::LessEq => LessEq,
            TokenKind::LessLess => Shl,
            TokenKind::Great => Great,
            TokenKind::GreatEq => GreatEq,
            TokenKind::GreatGreat => Shr,
            TokenKind::GreatGreatGreat => UShr,
            TokenKind::Instanceof => Instanceof,
            TokenKind::In => In,
            TokenKind::Satisfies => Satisfies,
            TokenKind::BangEq => NEq,
            TokenKind::BangEqEq => NEqEq,
            TokenKind::Caret => BitXor,
            TokenKind::AsteriskAsterisk => Exp,
            TokenKind::Comma => Comma,
            _ => {
                unreachable!("{:#?}", value)
            }
        }
    }
}

impl From<TokenKind> for super::PostfixUnaryOp {
    fn from(value: TokenKind) -> Self {
        use super::PostfixUnaryOp::*;
        match value {
            TokenKind::PlusPlus => PlusPlus,
            TokenKind::MinusMinus => MinusMinus,
            _ => {
                unreachable!("{:#?}", value)
            }
        }
    }
}

impl From<TokenKind> for super::PrefixUnaryOp {
    fn from(value: TokenKind) -> Self {
        use super::PrefixUnaryOp::*;
        match value {
            TokenKind::Plus => Plus,
            TokenKind::Minus => Minus,
            TokenKind::PlusPlus => PlusPlus,
            TokenKind::MinusMinus => MinusMinus,
            TokenKind::Tilde => Tilde,
            TokenKind::Excl => Excl,
            _ => {
                unreachable!("{:#?}", value)
            }
        }
    }
}

impl TryFrom<TokenKind> for super::ModifierKind {
    type Error = ();
    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        use super::ModifierKind::*;
        match value {
            TokenKind::Public => Ok(Public),
            TokenKind::Private => Ok(Private),
            TokenKind::Protected => Ok(Protected),
            TokenKind::Readonly => Ok(Readonly),
            TokenKind::Export => Ok(Export),
            // TODO: override
            TokenKind::Abstract => Ok(Abstract),
            TokenKind::Static => Ok(Static),
            TokenKind::Declare => Ok(Ambient),
            TokenKind::Default => Ok(Default),
            TokenKind::Const => Ok(Const),
            TokenKind::Async => Ok(Async),
            _ => Err(()),
        }
    }
}

impl From<TokenKind> for super::AssignOp {
    fn from(value: TokenKind) -> Self {
        use super::AssignOp::*;
        match value {
            TokenKind::Eq => Eq,
            TokenKind::PlusEq => AddEq,
            TokenKind::MinusEq => SubEq,
            TokenKind::AsteriskEq => MulEq,
            TokenKind::SlashEq => DivEq,
            TokenKind::PercentEq => ModEq,
            TokenKind::AmpEq => BitAndEq,
            TokenKind::PipeEq => BitOrEq,
            TokenKind::LessLessEq => ShlEq,
            TokenKind::GreatGreatEq => ShrEq,
            TokenKind::GreatGreatGreatEq => UShrEq,
            TokenKind::CaretEq => BitXorEq,
            _ => unreachable!(),
        }
    }
}

impl TryFrom<TokenKind> for super::TyOpKind {
    type Error = ();
    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        use super::TyOpKind::*;
        match value {
            TokenKind::Keyof => Ok(Keyof),
            TokenKind::Readonly => Ok(Readonly),
            TokenKind::Unique => Ok(Unique),
            _ => Err(()),
        }
    }
}

impl TokenKind {
    pub const fn prec(self) -> BinPrec {
        use TokenKind::*;
        match self {
            Pipe => BinPrec::BitwiseOR,
            Caret => BinPrec::BitwiseXOR,
            Amp => BinPrec::BitwiseAND,
            Less | Great | LessEq | GreatEq | Instanceof | In | As | Satisfies => {
                BinPrec::Relational
            }
            LessLess | GreatGreat | GreatGreatGreat => BinPrec::Shift,
            Plus | Minus => BinPrec::Additive,
            PipePipe => BinPrec::LogicalOr,
            AmpAmp => BinPrec::LogicalAnd,
            BangEq | BangEqEq | EqEq | EqEqEq => BinPrec::Eq,
            Asterisk | Slash | Percent => BinPrec::Multiplicative,
            AsteriskAsterisk => BinPrec::Exponentiation,
            _ => BinPrec::Invalid,
        }
    }

    pub const fn is_ident(&self) -> bool {
        if matches!(self, TokenKind::Ident) {
            return true;
        }

        // TODO: handle yield keyword and await keyword

        self.is_contextual_keyword() || self.is_strict_mode_reserved_word()
    }

    pub fn is_binding_ident(self) -> bool {
        matches!(self, TokenKind::Ident)
            || self.is_strict_mode_reserved_word()
            || self.is_contextual_keyword()
    }

    pub fn is_binding_ident_or_private_ident_or_pat(self) -> bool {
        matches!(self, TokenKind::LBrace | TokenKind::LBracket) || self.is_binding_ident()
    }

    pub const fn is_keyword(self) -> bool {
        let u = self as u8;
        u >= KEYWORD_TOKEN_START && u <= KEYWORD_TOKEN_END
    }

    pub const fn is_strict_mode_reserved_word(self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Implements | Interface | Let | Package | Private | Protected | Public | Static | Yield
        )
    }

    pub const fn is_contextual_keyword(self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Abstract |
            // Accessor |
            As |
            // Asserts |
            // Assert |
            // Any |
            Async |
            Await |
            // Boolean |
            Constructor |
            Declare |
            Get |
            Infer |
            Intrinsic |
            Is |
            Keyof |
            Module |
            Namespace |
            // Never |
            // Out |
            Readonly |
            // Require |
            // Object |
            // Satisfies |
            Set |
            // Symbol |
            Type |
            Undefined |
                 Unique |
                 // Unknown |
                 // Using |
                 From // Global |
                      // BigInt |
                      // Override |
        )
    }

    #[inline(always)]
    pub const fn is_ident_or_keyword(self) -> bool {
        matches!(self, TokenKind::Ident) || self.is_keyword()
    }

    #[inline(always)]
    pub const fn is_lit_prop_name(self) -> bool {
        use TokenKind::*;
        self.is_ident_or_keyword() || matches!(self, String | Number)
    }

    #[inline(always)]
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
        TryInto::<super::ModifierKind>::try_into(self).is_ok()
    }

    pub fn is_accessibility_modifier(self) -> bool {
        use TokenKind::*;
        matches!(self, Public | Private | Protected)
    }

    pub fn is_param_prop_modifier(self) -> bool {
        // TODO: readonly | override
        self.is_accessibility_modifier()
    }

    pub fn is_class_ele_modifier(self) -> bool {
        // TODO: override || accessor
        self.is_param_prop_modifier() || matches!(self, TokenKind::Static)
    }

    pub fn can_parse_module_export_name(self) -> bool {
        self.is_ident_or_keyword() || matches!(self, TokenKind::String)
    }

    pub fn is_in_or_of_keyword(self) -> bool {
        matches!(self, TokenKind::In | TokenKind::Of)
    }

    pub fn is_template(self) -> bool {
        matches!(
            self,
            TokenKind::TemplateHead
                | TokenKind::TemplateMiddle
                | TokenKind::TemplateTail
                | TokenKind::NoSubstitutionTemplate
        )
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
    /// `^`
    BitwiseXOR,
    /// `&`
    BitwiseAND,
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
    // `**`
    Exponentiation,
    Highest,
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct TokenFlags: u16 {
        const PRECEDING_LINE_BREAK          = 1 << 0;
        const UNTERMINATED                  = 1 << 2;
        const EXTENDED_UNICODE_ESCAPE       = 1 << 3;
        const SCIENTIFIC                    = 1 << 4;
        const HEX_SPECIFIER                 = 1 << 6;
        const BINARY_SPECIFIER              = 1 << 7;
        const OCTAL_SPECIFIER               = 1 << 8;
        /// `123_456`
        const CONTAINS_SEPARATOR            = 1 << 9;
        const UNICODE_ESCAPE                = 1 << 10;
        const CONTAINS_INVALID_ESCAPE       = 1 << 11;
        /// `0_1`
        const CONTAINS_INVALID_SEPARATOR    = 1 << 14;
        const NUMERIC_LITERAL_FLAGS         = Self::CONTAINS_SEPARATOR.bits() | Self::PRECEDING_LINE_BREAK.bits();
    }
}
