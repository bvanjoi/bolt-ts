// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalImportUnInstantiatedModuleNotReferencingInstanceNoConflict.ts`, Apache-2.0 License

//@compiler-options: target=esnext

namespace A {
    export interface X { s: string }
}

namespace B {
    var A = 1;
    import Y = A;
}
