// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/alwaysStrict.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: alwaysStrict

function f() {
    var arguments = [];
    //~^ ERROR: Invalid use of 'arguments' in strict mode.
}