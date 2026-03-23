// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classImplementsImportedInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M1 {
    export interface I {
        foo();
    }
}

namespace M2 {
    import T = M1.I;
    class C implements T {
        foo() {}
    }
}