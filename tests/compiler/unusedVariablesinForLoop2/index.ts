// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesinForLoop2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f1 () {
    for (const elem in ["a", "b", "c"]) {
        //~^ ERROR: 'elem' is declared but its value is never read.

    }
}