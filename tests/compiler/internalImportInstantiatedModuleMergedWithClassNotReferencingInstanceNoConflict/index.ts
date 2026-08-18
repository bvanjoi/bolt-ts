// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalImportInstantiatedModuleMergedWithClassNotReferencingInstanceNoConflict.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    aProp: string;
    //~^ ERROR: Property 'aProp' has no initializer and is not definitely assigned in the constructor.
}
namespace A {
    export interface X { s: string }
    export var a = 10;
}

namespace B {
    import Y = A;
}