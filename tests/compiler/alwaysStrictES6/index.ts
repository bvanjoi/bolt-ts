// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/alwaysStrictES6.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: alwaysStrict

function f() {
    var arguments = [];
    //~^ ERROR: Invalid use of 'arguments' in strict mode.
}