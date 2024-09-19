use crate::atoms::AtomId;

macro_rules! keyword {
    ($(($name:ident, $kw:literal)),* $(,)?) => {
        $(pub static $name: AtomId = AtomId::from_str($kw);)*
        pub static KEYWORDS: &[(&str, AtomId)] = &[$(($kw, $name),)*];
    };
}

keyword!(
    (KW_NULL, "null"),
    (KW_FALSE, "false"),
    (KW_TRUE, "true"),
    (KW_VAR, "var")
);

macro_rules! ident {
    ($(($name:ident, $ident:literal)),* $(,)?) => {
        $(pub static $name: AtomId = AtomId::from_str($ident);)*
        pub static IDENTIFIER: &[(&str, AtomId)] = &[$(($ident, $name),)*];
    };
}

ident!(
    (IDENT_UNDEFINED, "undefined"),
    (IDENT_ANY, "any"),
    (IDENT_NUMBER, "number")
);
