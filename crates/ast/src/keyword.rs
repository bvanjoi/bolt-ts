use bolt_ts_atom::{AtomId, gen_atoms};
use bolt_ts_utils::paste;

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
    (KW_DELETE, "delete"),
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
    (KW_UNDEFINED, "undefined"),
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
    (KW_INFER, "infer"),
    (KW_INTRINSIC, "intrinsic"),
    (KW_UNIQUE, "unique"),
    (KW_ASSERTS, "asserts"),
    (KW_TYPE, "type"),
);

gen_atoms!(
    IDENTIFIER,
    (IDENT_EMPTY, ""),
    (IDENT_LENGTH, "length"),
    (IDENT_ERROR, "error"),
    (IDENT_ANY, "any"),
    (IDENT_NUMBER, "number"),
    (IDENT_NUMBER_CLASS, "Number"),
    (IDENT_INFINITY, "Infinity"),
    (IDENT_ARRAY, "array"),
    (IDENT_ARRAY_CLASS, "Array"),
    (IDENT_READONLY_ARRAY_CLASS, "ReadonlyArray"),
    (IDENT_TEMPLATE_STRINGS_ARRAY_CLASS, "TemplateStringsArray"),
    (IDENT_STRING, "string"),
    (IDENT_STRING_CLASS, "String"),
    (IDENT_BOOLEAN, "boolean"),
    (IDENT_BOOLEAN_CLASS, "Boolean"),
    (IDENT_NEVER, "never"),
    (IDENT_UNKNOWN, "unknown"),
    (IDENT_BIGINT, "bigint"),
    (IDENT_OBJECT, "object"),
    (IDENT_OBJECT_CLASS, "Object"),
    (IDENT_SYMBOL, "symbol"),
    (IDENT_SYMBOL_CLASS, "Symbol"),
    (IDENT_FUNCTION_CLASS, "Function"),
    (IDENT_CALLABLE_FUNCTION_CLASS, "CallableFunction"),
    (IDENT_NEWABLE_FUNCTION_CLASS, "NewableFunction"),
    (IDENT_GLOBAL, "global"),
    (IDENT_GLOBAL_THIS, "globalThis"),
    (IDENT_ARGUMENTS, "arguments"),
    (IDENT_IARGUMENTS_CLASS, "IArguments"),
    (IDENT_REGEXP_CLASS, "RegExp"),
    (INTRINSIC_TYPE_UPPERCASE, "Uppercase"),
    (INTRINSIC_TYPE_LOWERCASE, "Lowercase"),
    (INTRINSIC_TYPE_CAPITALIZE, "Capitalize"),
    (INTRINSIC_TYPE_UNCAPITALIZE, "Uncapitalize"),
    (INTRINSIC_TYPE_NOINFER, "NoInfer"),
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
            | KW_UNDEFINED
            | KW_VOID
            | IDENT_SYMBOL
            | IDENT_OBJECT
            | IDENT_BIGINT
    )
}

pub fn is_prim_value_name(name: AtomId) -> bool {
    matches!(name, KW_NULL | KW_FALSE | KW_TRUE | KW_UNDEFINED)
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
            | KW_UNDEFINED
            | IDENT_OBJECT
    )
}

pub fn is_intrinsic_type_name(name: AtomId) -> bool {
    matches!(
        name,
        INTRINSIC_TYPE_UPPERCASE
            | INTRINSIC_TYPE_LOWERCASE
            | INTRINSIC_TYPE_CAPITALIZE
            | INTRINSIC_TYPE_UNCAPITALIZE
            | INTRINSIC_TYPE_NOINFER
    )
}
