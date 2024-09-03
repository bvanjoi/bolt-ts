use crate::atoms::AtomId;

macro_rules! keyword {
    ($(($name: ident, $kw:literal),)*) => {
        $(pub static $name: AtomId = AtomId::from_str($kw);)*
        pub static KEYWORDS: &[(&str, AtomId)] = &[$(($kw, $name),)*];
    };
}

keyword!((KW_FALSE, "false"), (KW_TRUE, "true"),);

pub static IDENTIFIER: &[(&str, AtomId)] = &[];
