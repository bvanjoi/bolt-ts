// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/blockScopedFunctionDeclarationES6.ts`, Apache-2.0 License

//@compiler-options: target=ES6

if (true) {
  function foo() { }
  foo();
}
foo();
//~^ ERROR: Cannot find name 'foo'.