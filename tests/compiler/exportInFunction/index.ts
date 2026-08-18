// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportInFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f() {
    export = 0;
    //~^ ERROR: An export assignment must be at the top level of a file or module declaration.
//~ERROR: '}' expected.