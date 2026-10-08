// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedInterfaceinNamespace3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    interface i1 {

    }

    export interface i2 {

    }

    interface i3 extends i1 {
      //~^ ERROR: 'i3' is declared but never used.

    }
}