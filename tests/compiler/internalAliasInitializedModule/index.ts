// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasInitializedModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace a {
    export namespace b {
        export class c {
        }
    }
}

namespace c {
    import b = a.b;
    export var x: b.c = new b.c();
}
