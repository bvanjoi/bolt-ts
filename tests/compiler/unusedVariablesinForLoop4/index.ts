// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesinForLoop4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f1 () {
    for (const elem of ["a", "b", "c"]) {
        elem;
        var x = 20;
        //~^ ERROR: 'x' is declared but its value is never read.
    }
}