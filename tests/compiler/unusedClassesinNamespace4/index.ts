// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedClassesinNamespace4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    class c1 {

    }

    export class c2 {

    }

    class c3 extends c1 {
    //~^ ERROR: 'c3' is declared but never used.

    }
}
