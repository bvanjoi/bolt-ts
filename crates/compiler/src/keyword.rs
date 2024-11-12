use crate::atoms::AtomId;

macro_rules! keyword {
    ($(($name:ident: $kw:literal)),* $(,)?) => {
        $(pub static $name: AtomId = AtomId::from_str($kw);)*
        pub static KEYWORDS: &[(&str, AtomId)] = &[$(($kw, $name),)*];
    };
}

keyword!(
    (KW_NULL: "null"),
    (KW_FALSE: "false"),
    (KW_TRUE: "true"),
    (KW_VAR: "var"),
    (KW_LET: "let"),
    (KW_CONST: "const"),
    (KW_FUNCTION: "function"),
    (KW_RETURN: "return"),
    (KW_IF: "if"),
    (KW_ELSE: "else"),
    (KW_CLASS: "class"),
    (KW_EXTENDS: "extends"),
    (KW_NEW: "new"),
    (KW_ASYNC: "async"),
    (KW_THIS: "this"),
    (KW_STATIC: "static"),
    // ts keywords
    (KW_IMPLEMENTS: "implements"),
    (KW_INTERFACE: "interface"),
    (KW_ABSTRACT: "abstract"),
    (KW_PUBLIC: "public"),
    (KW_AS: "as"),
);

macro_rules! ident {
    ($(($name:ident: $ident:literal)),* $(,)?) => {
        $(pub static $name: AtomId = AtomId::from_str($ident);)*
        pub static IDENTIFIER: &[(&str, AtomId)] = &[$(($ident, $name),)*];
    };
}

ident!(
    (IDENT_EMPTY: ""),
    (IDENT_ERROR: "error"),
    (IDENT_UNDEFINED: "undefined"),
    (IDENT_ANY: "any"),
    (IDENT_VOID: "void"),
    (IDENT_NUMBER: "number"),
    (IDENT_ARRAY: "array"),
    (IDENT_ARRAY_CLASS: "Array"),
    (IDENT_STRING: "string"),
    (IDENT_BOOLEAN: "boolean"),
);
