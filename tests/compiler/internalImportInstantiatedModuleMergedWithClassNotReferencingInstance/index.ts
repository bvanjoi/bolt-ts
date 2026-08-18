// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalImportInstantiatedModuleMergedWithClassNotReferencingInstance.ts`, Apache-2.0 License

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
    var A = 1;
    import Y = A;
    //~^ ERROR: Module 'A' is hidden by a local declaration with the same name.
}