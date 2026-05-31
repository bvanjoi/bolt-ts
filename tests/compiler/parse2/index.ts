// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parse2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo() {
 foo(
}
//~^ ERROR: Argument expression expected.
//~| ERROR: Expected ')'.