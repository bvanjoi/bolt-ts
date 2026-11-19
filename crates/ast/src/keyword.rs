use bolt_ts_atom::{Atom, prefilled_atom_map};

pub fn is_prim_ty_name(name: Atom) -> bool {
    matches!(
        name,
        IDENT_ANY
            | IDENT_UNKNOWN
            | IDENT_NUMBER
            | IDENT_BIGINT
            | IDENT_STRING
            | IDENT_BOOLEAN
            | IDENT_SYMBOL
            | IDENT_OBJECT
            | KW_UNDEFINED
            | KW_NULL
            | IDENT_NEVER
            | KW_VOID
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
            KW_WITH:            ["with", 37],
            KW_DO:              ["do", 38],
            KW_SWITCH:          ["switch", 39],
            KW_CASE:            ["case", 40],
            KW_BREAK:           ["break", 41],
            KW_CONTINUE:        ["continue", 42],
            KW_INSTANCEOF:      ["instanceof", 43],
            KW_VOID:            ["void", 44],
            KW_UNDEFINED:       ["undefined", 45],
            KW_IN:              ["in", 46],
            // ts keywords
            KW_IMPLEMENTS:      ["implements", 47],
            KW_INTERFACE:       ["interface", 48],
            KW_ABSTRACT:        ["abstract", 49],
            KW_PUBLIC:          ["public", 50],
            KW_PROTECTED:       ["protected", 51],
            KW_PRIVATE:         ["private", 52],
            KW_AS:              ["as", 53],
            KW_IS:              ["is", 54],
            KW_DECLARE:         ["declare", 55],
            KW_MODULE:          ["module", 56],
            KW_NAMESPACE:       ["namespace", 57],
            KW_ENUM:            ["enum", 58],
            KW_READONLY:        ["readonly", 59],
            KW_SATISFIES:       ["satisfies", 60],
            KW_KEYOF:           ["keyof", 61],
            KW_INFER:           ["infer", 62],
            KW_INTRINSIC:       ["intrinsic", 63],
            KW_UNIQUE:          ["unique", 64],
            KW_ASSERTS:         ["asserts", 65],
            KW_TYPE:            ["type", 66],
            KW_OVERRIDE:        ["override", 67],
            KW_ACCESSOR:        ["accessor", 68]
        },
        IDENTIFIER: {
            IDENT_EMPTY:                            ["", 69],
            IDENT_LENGTH:                           ["length", 70],
            IDENT_ERROR:                            ["error", 71],
            IDENT_ANY:                              ["any", 72],
            IDENT_NUMBER:                           ["number", 73],
            IDENT_NUMBER_CLASS:                     ["Number", 74],
            IDENT_INFINITY:                         ["Infinity", 75],
            IDENT_ARRAY:                            ["array", 76],
            IDENT_ARRAY_CLASS:                      ["Array", 77],
            IDENT_READONLY_ARRAY_CLASS:             ["ReadonlyArray", 78],
            IDENT_TEMPLATE_STRINGS_ARRAY_CLASS:     ["TemplateStringsArray", 79],
            IDENT_STRING:                           ["string", 80],
            IDENT_STRING_CLASS:                     ["String", 81],
            IDENT_BOOLEAN:                          ["boolean", 82],
            IDENT_BOOLEAN_CLASS:                    ["Boolean", 83],
            IDENT_NEVER:                            ["never", 84],
            IDENT_UNKNOWN:                          ["unknown", 85],
            IDENT_BIGINT:                           ["bigint", 86],
            IDENT_BIGINT_CLASS:                     ["BigInt", 87],
            IDENT_OBJECT:                           ["object", 88],
            IDENT_OBJECT_CLASS:                     ["Object", 89],
            IDENT_SYMBOL:                           ["symbol", 90],
            IDENT_SYMBOL_CLASS:                     ["Symbol", 91],
            IDENT_FUNCTION_CLASS:                   ["Function", 92],
            IDENT_CALLABLE_FUNCTION_CLASS:          ["CallableFunction", 93],
            IDENT_NEWABLE_FUNCTION_CLASS:           ["NewableFunction", 94],
            IDENT_GLOBAL:                           ["global", 95],
            IDENT_GLOBAL_THIS:                      ["globalThis", 96],
            IDENT_ARGUMENTS:                        ["arguments", 97],
            IDENT_IARGUMENTS_CLASS:                 ["IArguments", 98],
            IDENT_THIS_TYPE_CLASS:                  ["ThisType", 99],
            IDENT_REGEXP_CLASS:                     ["RegExp", 100],
            IDENT_PROTOTYPE:                        ["prototype", 101],
            IDENT_EXTRACT:                          ["Extract", 102],
            IDENT_EVAL:                             ["eval", 103],
            IDENT_NON_NULLABLE:                     ["NonNullable", 104],
            IDENT_OMIT:                             ["Omit", 105],
            IDENT_AWAITED:                          ["Awaited", 106],
            INTRINSIC_TYPE_UPPERCASE:               ["Uppercase", 107],
            INTRINSIC_TYPE_LOWERCASE:               ["Lowercase", 108],
            INTRINSIC_TYPE_CAPITALIZE:              ["Capitalize", 109],
            INTRINSIC_TYPE_UNCAPITALIZE:            ["Uncapitalize", 110],
            INTRINSIC_TYPE_NOINFER:                 ["NoInfer", 111],
            IDENT_PUSH:                             ["push", 112],
            IDENT_UNSHIFT:                          ["unshift", 113],
            IDENT_OPTIONAL:                         ["optional", 114],
            IDENT_THEN:                             ["then", 115],
            IDENT_PROMISE:                          ["Promise", 116],
            IDENT_PROMISE_LIKE:                     ["PromiseLike", 117],
            IDENT_ITERATOR:                         ["iterator", 118],
            IDENT_ITERATOR_CLASS:                   ["Iterator", 119],
            IDENT_ASYNC_ITERATOR:                   ["asyncIterator", 120],
            IDENT_ASYNC_ITERATOR_CLASS:             ["AsyncIterator", 121],
            IDENT_ASYNC_ITERABLE:                   ["AsyncIterable", 122],
            IDENT_ASYNC_ITERABLE_ITERATOR:          ["AsyncIterableIterator", 123],
            IDENT_READABLE_STREAM_ASYNC_ITERATOR:   ["ReadableStreamAsyncIterator", 124],
            IDENT_ASYNC_ITERATOR_OBJECT:            ["AsyncIteratorObject", 125],
            IDENT_ITERABLE:                         ["Iterable", 126],
            IDENT_ITERABLE_ITERATOR:                ["IterableIterator", 127],
            IDENT_ITERATOR_OBJECT:                  ["IteratorObject", 128],
            IDENT_GENERATOR:                        ["Generator", 129],
            IDENT_ASYNC_GENERATOR:                  ["AsyncGenerator", 130],
            IDENT_ARRAY_ITERATOR:                   ["ArrayIterator", 131],
            IDENT_MAP_ITERATOR:                     ["MapIterator", 132],
            IDENT_SET_ITERATOR:                     ["SetIterator", 133],
            IDENT_STRING_ITERATOR:                  ["StringIterator", 134],
            IDENT_NEXT:                             ["next", 135],
            IDENT_ITERATOR_YIELD_RESULT:            ["IteratorYieldResult", 136],
            IDENT_ITERATOR_RETURN_RESULT:           ["IteratorReturnResult", 137],
            IDENT_DONE:                             ["done", 138],
            IDENT_VALUE:                            ["value", 139],
            IDENT_REQUIRE:                          ["require", 140],
            IDENT_RECORD:                           ["Record", 141],
            IDENT_NAN:                              ["NaN", 142],
            IDENT_TARGET:                           ["target", 143],
            IDENT_HAS_OWN_PROPERTY:                 ["hasOwnProperty", 144],
            IDENT_DEFINE_PROPERTY:                  ["defineProperty", 145],
            IDENT_FLOAT32_ARRAY_CLASS:              ["Float32Array", 146],
            IDENT_FLOAT64_ARRAY_CLASS:              ["Float64Array", 147],
            IDENT_INT8_ARRAY_CLASS:                 ["Int8Array", 148],
            IDENT_INT16_ARRAY_CLASS:                ["Int16Array", 149],
            IDENT_INT32_ARRAY_CLASS:                ["Int32Array", 150],
            IDENT_UINT8_ARRAY_CLASS:                ["Uint8Array", 151],
            IDENT_UINT8_CLAMPED_ARRAY_CLASS:        ["Uint8ClampedArray", 152],
            IDENT_UINT16_ARRAY_CLASS:               ["Uint16Array", 153],
            IDENT_UINT32_ARRAY_CLASS:               ["Uint32Array", 154],
            IDENT_NODE_LIST_CLASS:                  ["NodeList", 155],
            IDENT_HAS_INSTANCE:                     ["hasInstance", 156],
        },
        DIRECTIVES: {
            DIRECTIVE_USE_STRICT:                   ["use strict", 157]
        },
        SPECIAL_IDENTIFIER: {
            SPECIAL_IDENT_ERROR:                    ["<error>", 158]
        },
        NUMBER: {
            NUMBER_ZERO:                            ["0", 159],
            NUMBER_NEGATIVE_INFINITY:               ["-Infinity", 160]
        },
    }
);

pub fn is_push_or_unshift(name: Atom) -> bool {
    matches!(name, IDENT_PUSH | IDENT_UNSHIFT)
}
