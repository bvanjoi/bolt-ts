// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedFunctionsinNamespaces1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    function function1() {
      //~^ ERROR: 'function1' is declared but its value is never read.
    }
}