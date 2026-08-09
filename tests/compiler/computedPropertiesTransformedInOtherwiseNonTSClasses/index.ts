// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/computedPropertiesTransformedInOtherwiseNonTSClasses.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es6]

namespace NS { 
    export const x = Symbol();

    class NotTransformed { 
        [NS.x]: number;
        //~^ ERROR: Property 'computed' has no initializer and is not definitely assigned in the constructor.
    }
}
