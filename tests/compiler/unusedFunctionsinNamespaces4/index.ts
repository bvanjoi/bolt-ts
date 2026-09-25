// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedFunctionsinNamespaces4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    var function1 = function() {
      //~^ ERROR: 'function1' is declared but its value is never read.
    }

    export function function2() {

    }
}
