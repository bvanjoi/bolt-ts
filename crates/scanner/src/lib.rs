mod comments;
mod scan_integer;
mod scan_str;
mod unicode;

pub use self::comments::{Comment, CommentId, Comments, LeadingTrailingComments};
pub use self::comments::{CommentKind, iterate_comment_ranges};
pub use self::scan_integer::parse_integer;
pub use self::scan_str::is_ascii_identifier_part;
pub use self::scan_str::is_ascii_identifier_start;
pub use self::scan_str::is_identifier_part;
pub use self::scan_str::is_identifier_start;
pub use self::scan_str::is_non_ascii_identifier_start;
pub use self::scan_str::non_ascii_character_code;
pub use self::scan_str::utf16_encode_as_bytes;

#[inline(always)]
pub fn is_line_break(ch: u8) -> bool {
    ch == b'\n' || ch == b'\r'
}

#[derive(Debug, Clone, Copy)]
pub enum TokenValue {
    Number { value: f64 },
    Ident { value: bolt_ts_atom::Atom },
}

impl TokenValue {
    pub fn number(self) -> f64 {
        match self {
            TokenValue::Number { value } => value,
            TokenValue::Ident { .. } => unreachable!(),
        }
    }

    pub fn ident(self) -> bolt_ts_atom::Atom {
        match self {
            TokenValue::Ident { value } => value,
            TokenValue::Number { .. } => unreachable!(),
        }
    }
}
