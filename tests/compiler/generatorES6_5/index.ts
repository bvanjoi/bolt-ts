// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/generatorES6_5.ts`, Apache-2.0 License

//@compiler-options: target=es6
function* foo() {
    yield a ? b : c;
    //~^ ERROR: Cannot find name 'a'.
    //~| ERROR: Cannot find name 'b'.
    //~| ERROR: Cannot find name 'c'.
}