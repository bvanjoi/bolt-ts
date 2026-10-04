// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasVar.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace a {
    export var x = 10;
}

namespace c {
    import b = a.x;
    export var bVal = b;
}
