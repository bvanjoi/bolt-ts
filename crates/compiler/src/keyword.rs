use crate::atoms::AtomId;

macro_rules! keyword {
    ($(($name:ident, $name_str: ident, $kw:literal)),* $(,)?) => {
        $(pub static $name: AtomId = AtomId::from_str($kw);)*
        $(pub static $name_str: &'static str = $kw;)*
        pub static KEYWORDS: &[(&str, AtomId)] = &[$(($kw, $name),)*];
    };
}

keyword!(
    (KW_NULL, KW_NULL_STR, "null"),
    (KW_FALSE, KW_FALSE_STR, "false"),
    (KW_TRUE, KW_TRUE_STR, "true"),
    (KW_VAR, KW_VAR_STR, "var"),
    (KW_LET, KW_LET_STR, "let"),
    (KW_CONST, KW_CONST_STR, "const"),
    (KW_FUNCTION, KW_FUNCTION_STR, "function"),
    (KW_RETURN, KW_RETURN_STR, "return"),
    (KW_IF, KW_IF_STR, "if"),
    (KW_ELSE, KW_ELSE_STR, "else"),
    (KW_CLASS, KW_CLASS_STR, "class"),
    (KW_EXTENDS, KW_EXTENDS_STR, "extends"),
    (KW_NEW, KW_NEW_STR, "new"),
    (KW_ASYNC, KW_ASYNC_STR, "async"),
    (KW_THIS, KW_THIS_STR, "this"),
    (KW_STATIC, KW_STATIC_STR, "static"),
    (KW_CONSTRUCTOR, KW_CONSTRUCTOR_STR, "constructor"),
    (KW_SUPER, KW_SUPER_STR, "super"),
    (KW_GET, KW_GET_STR, "get"),
    (KW_SET, KW_SET_STR, "set"),
    (KW_IN, KW_IN_STR, "in"),
    // ts keywords
    (KW_IMPLEMENTS, KW_IMPLEMENTS_STR, "implements"),
    (KW_INTERFACE, KW_INTERFACE_STR, "interface"),
    (KW_ABSTRACT, KW_ABSTRACT_STR, "abstract"),
    (KW_PUBLIC, KW_PUBLIC_STR, "public"),
    (KW_PRIVATE, KW_PRIVATE_STR, "private"),
    (KW_AS, KW_AS_STR, "as"),
    (KW_DECLARE, KW_DECLARE_STR, "declare"),
    (KW_TYPE, KW_TYPE_STR, "type")
);

macro_rules! ident {
    ($(($name:ident, $name_str: ident, $ident:literal)),* $(,)?) => {
        $(pub static $name: AtomId = AtomId::from_str($ident);)*
        $(pub static $name_str: &'static str = $ident;)*
        pub static IDENTIFIER: &[(&str, AtomId)] = &[$(($ident, $name),)*];
    };
}

ident!(
    (IDENT_EMPTY, IDENT_EMPTY_STR, ""),
    (IDENT_ERROR, IDENT_ERROR_STR, "error"),
    (IDENT_UNDEFINED, IDENT_UNDEFINED_STR, "undefined"),
    (IDENT_ANY, IDENT_ANY_STR, "any"),
    (IDENT_VOID, IDENT_VOID_STR, "void"),
    (IDENT_NUMBER, IDENT_NUMBER_STR, "number"),
    (IDENT_NUMBER_CLASS, IDENT_NUMBER_CLASS_STR, "Number"),
    (IDENT_ARRAY, IDENT_ARRAY_STR, "array"),
    (IDENT_ARRAY_CLASS, IDENT_ARRAY_CLASS_STR, "Array"),
    (IDENT_STRING, IDENT_STRING_STR, "string"),
    (IDENT_BOOLEAN, IDENT_BOOLEAN_STR, "boolean"),
    (IDENT_NEVER, IDENT_NEVER_STR, "never"),
    (IDENT_UNKNOWN, IDENT_UNKNOWN_STR, "unknown"),
);
