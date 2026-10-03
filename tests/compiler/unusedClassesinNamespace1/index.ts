// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedClassesinNamespace1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    class c1 {
      //~^ ERROR: 'c1' is declared but never used.

    }
}
