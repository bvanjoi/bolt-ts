use bolt_ts_atom::{Atom, prefilled_atom_map};

pub fn is_prim_ty_name(name: Atom) -> bool {
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

pub fn is_prim_value_name(name: Atom) -> bool {
    matches!(name, KW_NULL | KW_FALSE | KW_TRUE | KW_UNDEFINED)
}

pub fn is_reserved_type_name(name: Atom) -> bool {
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

pub fn is_intrinsic_type_name(name: Atom) -> bool {
    matches!(
        name,
        INTRINSIC_TYPE_UPPERCASE
            | INTRINSIC_TYPE_LOWERCASE
            | INTRINSIC_TYPE_CAPITALIZE
            | INTRINSIC_TYPE_UNCAPITALIZE
            | INTRINSIC_TYPE_NOINFER
    )
}

prefilled_atom_map!(
    init_atom_map,
    {
        KEYWORDS: {
            KW_NULL:            ["null", 0],
            KW_FALSE:           ["false", 1],
            KW_TRUE:            ["true", 2],
            KW_VAR:             ["var", 3],
            KW_LET:             ["let", 4],
            KW_CONST:           ["const", 5],
            KW_FUNCTION:        ["function", 6],
            KW_RETURN:          ["return", 7],
            KW_IF:              ["if", 8],
            KW_ELSE:            ["else", 9],
            KW_CLASS:           ["class", 10],
            KW_EXTENDS:         ["extends", 11],
            KW_NEW:             ["new", 12],
            KW_ASYNC:           ["async", 13],
            KW_AWAIT:           ["await", 14],
            KW_THIS:            ["this", 15],
            KW_STATIC:          ["static", 16],
            KW_CONSTRUCTOR:     ["constructor", 17],
            KW_SUPER:           ["super", 18],
            KW_GET:             ["get", 19],
            KW_SET:             ["set", 20],
            KW_IMPORT:          ["import", 21],
            KW_EXPORT:          ["export", 22],
            KW_FROM:            ["from", 23],
            KW_DEFAULT:         ["default", 24],
            KW_THROW:           ["throw", 25],
            KW_TRY:             ["try", 26],
            KW_CATCH:           ["catch", 27],
            KW_FINALLY:         ["finally", 28],
            KW_DEBUGGER:        ["debugger", 29],
            KW_DELETE:          ["delete", 30],
            KW_TYPEOF:          ["typeof", 31],
            KW_PACKAGE:         ["package", 32],
            KW_YIELD:           ["yield", 33],
            KW_FOR:             ["for", 34],
            KW_OF:              ["of", 35],
            KW_WHILE:           ["while", 36],
            KW_DO:              ["do", 37],
            KW_SWITCH:          ["switch", 38],
            KW_CASE:            ["case", 39],
            KW_BREAK:           ["break", 40],
            KW_CONTINUE:        ["continue", 41],
            KW_INSTANCEOF:      ["instanceof", 42],
            KW_VOID:            ["void", 43],
            KW_UNDEFINED:       ["undefined", 44],
            KW_IN:              ["in", 45],
            // ts keywords
            KW_IMPLEMENTS:      ["implements", 46],
            KW_INTERFACE:       ["interface", 47],
            KW_ABSTRACT:        ["abstract", 48],
            KW_PUBLIC:          ["public", 49],
            KW_PROTECTED:       ["protected", 50],
            KW_PRIVATE:         ["private", 51],
            KW_AS:              ["as", 52],
            KW_IS:              ["is", 53],
            KW_DECLARE:         ["declare", 54],
            KW_MODULE:          ["module", 55],
            KW_NAMESPACE:       ["namespace", 56],
            KW_ENUM:            ["enum", 57],
            KW_READONLY:        ["readonly", 58],
            KW_SATISFIES:       ["satisfies", 59],
            KW_KEYOF:           ["keyof", 60],
            KW_INFER:           ["infer", 61],
            KW_INTRINSIC:       ["intrinsic", 62],
            KW_UNIQUE:          ["unique", 63],
            KW_ASSERTS:         ["asserts", 64],
            KW_TYPE:            ["type", 65]
        },
        IDENTIFIER: {
            IDENT_EMPTY:                            ["", 66],
            IDENT_LENGTH:                           ["length", 67],
            IDENT_ERROR:                            ["error", 68],
            IDENT_ANY:                              ["any", 69],
            IDENT_NUMBER:                           ["number", 70],
            IDENT_NUMBER_CLASS:                     ["Number", 71],
            IDENT_INFINITY:                         ["Infinity", 72],
            IDENT_ARRAY:                            ["array", 73],
            IDENT_ARRAY_CLASS:                      ["Array", 74],
            IDENT_READONLY_ARRAY_CLASS:             ["ReadonlyArray", 75],
            IDENT_TEMPLATE_STRINGS_ARRAY_CLASS:     ["TemplateStringsArray", 76],
            IDENT_STRING:                           ["string", 77],
            IDENT_STRING_CLASS:                     ["String", 78],
            IDENT_BOOLEAN:                          ["boolean", 79],
            IDENT_BOOLEAN_CLASS:                    ["Boolean", 80],
            IDENT_NEVER:                            ["never", 81],
            IDENT_UNKNOWN:                          ["unknown", 82],
            IDENT_BIGINT:                           ["bigint", 83],
            IDENT_OBJECT:                           ["object", 84],
            IDENT_OBJECT_CLASS:                     ["Object", 85],
            IDENT_SYMBOL:                           ["symbol", 86],
            IDENT_SYMBOL_CLASS:                     ["Symbol", 87],
            IDENT_FUNCTION_CLASS:                   ["Function", 88],
            IDENT_CALLABLE_FUNCTION_CLASS:          ["CallableFunction", 89],
            IDENT_NEWABLE_FUNCTION_CLASS:           ["NewableFunction", 90],
            IDENT_GLOBAL:                           ["global", 91],
            IDENT_GLOBAL_THIS:                      ["globalThis", 92],
            IDENT_ARGUMENTS:                        ["arguments", 93],
            IDENT_IARGUMENTS_CLASS:                 ["IArguments", 94],
            IDENT_REGEXP_CLASS:                     ["RegExp", 95],
            IDENT_PROTOTYPE:                        ["prototype", 96],
            IDENT_EXTRACT:                          ["Extract", 97],
            IDENT_EVAL:                             ["eval", 98],
            IDENT_NON_NULLABLE:                     ["NonNullable", 99],
            INTRINSIC_TYPE_UPPERCASE:               ["Uppercase", 100],
            INTRINSIC_TYPE_LOWERCASE:               ["Lowercase", 101],
            INTRINSIC_TYPE_CAPITALIZE:              ["Capitalize", 102],
            INTRINSIC_TYPE_UNCAPITALIZE:            ["Uncapitalize", 103],
            INTRINSIC_TYPE_NOINFER:                 ["NoInfer", 104]
        },
        DIRECTIVES: {
            DIRECTIVE_USE_STRICT:                   ["use strict", 105]
        },
        SPECIAL_IDENTIFIER: {
            SPECIAL_IDENT_ERROR:                    ["<error>", 106]
        },
    }
);
