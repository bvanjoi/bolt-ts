// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportImportNonInstantiatedModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    export interface I { x: number }
}

namespace B {
    export import A1 = A
    
}

var x: B.A1.I = { x: 1 };