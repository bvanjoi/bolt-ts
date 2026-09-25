// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesinForLoop.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f1 () {
    for(var i = 0; ;) {
      //~^ ERROR: 'i' is declared but its value is never read.

    }
}