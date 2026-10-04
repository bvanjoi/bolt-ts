// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace a {
    export class c {
    }
}

namespace c {
    import b = a.c;
    export var x: b = new b();
}