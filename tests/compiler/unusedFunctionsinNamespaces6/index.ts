// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedFunctionsinNamespaces6.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    var function1 = function() {
    }

    export function function2() {

    }

    function function3() {
        function1();
    }

    function function4() {
      //~^ ERROR: 'function4' is declared but its value is never read.
    }

    export let a = function3;
}
