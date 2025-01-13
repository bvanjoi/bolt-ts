use bolt_ts_atom::{gen_atoms, paste, AtomId};

gen_atoms!(
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
    (KW_AWAIT, "await"),
    (KW_THIS, "this"),
    (KW_STATIC, "static"),
    (KW_CONSTRUCTOR, "constructor"),
    (KW_SUPER, "super"),
    (KW_GET, "get"),
    (KW_SET, "set"),
    (KW_IMPORT, "import"),
    (KW_EXPORT, "export"),
    (KW_FROM, "from"),
    (KW_DEFAULT, "default"),
    (KW_THROW, "throw"),
    (KW_TRY, "try"),
    (KW_CATCH, "catch"),
    (KW_FINALLY, "finally"),
    (KW_DEBUGGER, "debugger"),
    (KW_TYPEOF, "typeof"),
    (KW_PACKAGE, "package"),
    (KW_YIELD, "yield"),
    (KW_FOR, "for"),
    (KW_OF, "of"),
    (KW_WHILE, "while"),
    (KW_DO, "do"),
    (KW_SWITCH, "switch"),
    (KW_CASE, "case"),
    (KW_BREAK, "break"),
    (KW_CONTINUE, "continue"),
    (KW_INSTANCEOF, "instanceof"),
    (KW_VOID, "void"),
    (KW_IN, "in"),
    // ts keywords
    (KW_IMPLEMENTS, "implements"),
    (KW_INTERFACE, "interface"),
    (KW_ABSTRACT, "abstract"),
    (KW_PUBLIC, "public"),
    (KW_PROTECTED, "protected"),
    (KW_PRIVATE, "private"),
    (KW_AS, "as"),
    (KW_IS, "is"),
    (KW_DECLARE, "declare"),
    (KW_MODULE, "module"),
    (KW_NAMESPACE, "namespace"),
    (KW_ENUM, "enum"),
    (KW_READONLY, "readonly"),
    (KW_SATISFIES, "satisfies"),
    (KW_KEYOF, "keyof"),
    (KW_TYPE, "type"),
);

gen_atoms!(
    IDENTIFIER,
    (IDENT_EMPTY, ""),
    (IDENT_LENGTH, "length"),
    (IDENT_ERROR, "error"),
    (IDENT_UNDEFINED, "undefined"),
    (IDENT_ANY, "any"),
    (IDENT_NUMBER, "number"),
    (IDENT_NUMBER_CLASS, "Number"),
    (IDENT_ARRAY, "array"),
    (IDENT_ARRAY_CLASS, "Array"),
    (IDENT_STRING, "string"),
    (IDENT_STRING_CLASS, "String"),
    (IDENT_BOOLEAN, "boolean"),
    (IDENT_NEVER, "never"),
    (IDENT_UNKNOWN, "unknown"),
    (IDENT_BIGINT, "bitint"),
    (IDENT_OBJECT, "object"),
    (IDENT_OBJECT_CLASS, "Object"),
    (IDENT_SYMBOL, "symbol"),
    (IDENT_FUNCTION_CLASS, "Function"),
    (IDENT_CALLABLE_FUNCTION_CLASS, "CallableFunction"),
    (IDENT_NEWABLE_FUNCTION_CLASS, "NewableFunction"),
    (IDENT_GLOBAL, "global"),
);

pub fn is_prim_ty_name(name: AtomId) -> bool {
    matches!(
        name,
        IDENT_ANY
            | KW_NULL
            | IDENT_NUMBER
            | IDENT_STRING
            | IDENT_BOOLEAN
            | IDENT_NEVER
            | IDENT_UNKNOWN
            | KW_VOID
            | IDENT_UNDEFINED
    )
}

pub fn is_prim_value_name(name: AtomId) -> bool {
    matches!(name, KW_NULL | KW_FALSE | KW_TRUE | IDENT_UNDEFINED)
}

pub fn is_reserved_type_name(name: AtomId) -> bool {
    matches!(
        name,
        IDENT_ANY
            | IDENT_UNKNOWN
            | IDENT_NEVER
            | IDENT_NUMBER
            | IDENT_BIGINT
            | IDENT_BOOLEAN
            | IDENT_STRING
            | KW_VOID
            | IDENT_OBJECT
            | IDENT_UNDEFINED
    )
}
