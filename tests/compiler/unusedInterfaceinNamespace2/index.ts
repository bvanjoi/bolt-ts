// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedInterfaceinNamespace2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    interface i1 {
      //~^ ERROR: 'i1' is declared but never used.

    }

    export interface i2 {

    }
}