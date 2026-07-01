// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace a {
    export interface I {
    }
}

namespace c {
    import b = a.I;
    export var x: b;
}
