// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedFunctionsinNamespaces3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    var function1 = function(param1:string) {
      //~^ ERROR: 'function1' is declared but its value is never read.
      //~| ERROR: 'param1' is declared but its value is never read.
    }
}
