// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classExpressionWithResolutionOfNamespaceOfSameName01.ts`, Apache-2.0 License

namespace C {
    export interface type {
    }
}

var x = class C {
    prop: C.type;
    //~^ ERROR: Property 'prop' has no initializer and is not definitely assigned in the constructor.
}
