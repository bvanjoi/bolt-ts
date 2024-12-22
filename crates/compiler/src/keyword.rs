use crate::atoms::AtomId;
macro_rules! gen {
    ($owner: ident, $(($name:ident, $lit:literal)),* $(,)?) => {
        gen!($(($name, $lit)),*);
        pub const $owner: &[(&str, AtomId)] = &[$(($lit, $name),)*];
    };
    ($(($name:ident, $lit:literal)),* $(,)?) => {
        paste::paste! {
            $(pub const [<$name _STR>]: &str = $lit;)*
            $(pub const $name: AtomId = AtomId::from_str([<$name _STR>]);)*
        }
    };
}

gen!(
    KEYWORDS,
    (KW_NULL, "null"),
    (KW_FALSE, "false"),
    (KW_TRUE, "true"),
    (KW_VAR, "var"),
    (KW_LET, "let"),
    (KW_CONST, "const"),
    (KW_FUNCTION, "function"),
    (KW_RETURN, "return"),
    (KW_IF, "if"),
    (KW_ELSE, "else"),
    (KW_CLASS, "class"),
    (KW_EXTENDS, "extends"),
    (KW_NEW, "new"),
    (KW_ASYNC, "async"),
    (KW_THIS, "this"),
    (KW_STATIC, "static"),
    (KW_CONSTRUCTOR, "constructor"),
    (KW_SUPER, "super"),
    (KW_GET, "get"),
    (KW_SET, "set"),
    (KW_IMPORT, "import"),
    (KW_EXPORT, "export"),
    (KW_DEFAULT, "default"),
    (KW_THROW, "throw"),
    (KW_TRY, "try"),
    (KW_CATCH, "catch"),
    (KW_FINALLY, "finally"),
    (KW_DEBUGGER, "debugger"),
    (KW_TYPEOF, "typeof"),
    (KW_IN, "in"),
    // ts keywords
    (KW_IMPLEMENTS, "implements"),
    (KW_INTERFACE, "interface"),
    (KW_ABSTRACT, "abstract"),
    (KW_PUBLIC, "public"),
    (KW_PRIVATE, "private"),
    (KW_AS, "as"),
    (KW_IS, "is"),
    (KW_DECLARE, "declare"),
    (KW_MODULE, "module"),
    (KW_NAMESPACE, "namespace"),
    (KW_ENUM, "enum"),
    (KW_READONLY, "readonly"),
    (KW_TYPE, "type"),
);

gen!(
    IDENTIFIER,
    (IDENT_EMPTY, ""),
    (IDENT_LENGTH, "length"),
    (IDENT_ERROR, "error"),
    (IDENT_UNDEFINED, "undefined"),
    (IDENT_ANY, "any"),
    (IDENT_VOID, "void"),
    (IDENT_NUMBER, "number"),
    (IDENT_NUMBER_CLASS, "Number"),
    (IDENT_ARRAY, "array"),
    (IDENT_ARRAY_CLASS, "Array"),
    (IDENT_STRING, "string"),
    (IDENT_STRING_CLASS, "String"),
    (IDENT_BOOLEAN, "boolean"),
    (IDENT_NEVER, "never"),
    (IDENT_UNKNOWN, "unknown"),
);

pub fn is_prim_ty_name(name: AtomId) -> bool {
    matches!(
        name,
        IDENT_ANY
            | IDENT_NUMBER
            | IDENT_STRING
            | IDENT_BOOLEAN
            | IDENT_NEVER
            | IDENT_UNKNOWN
            | IDENT_VOID
            | IDENT_UNDEFINED
    )
}

pub fn is_prim_value_name(name: AtomId) -> bool {
    matches!(name, KW_NULL | KW_FALSE | KW_TRUE | IDENT_UNDEFINED)
}
