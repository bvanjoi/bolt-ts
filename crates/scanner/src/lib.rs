mod scan_integer;
mod unicode;

pub use self::scan_integer::parse_integer;
pub use self::unicode::{is_unicode_es5_identifier_part, is_unicode_es5_identifier_start};
pub use self::unicode::{is_unicode_esnext_identifier_part, is_unicode_esnext_identifier_start};

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
