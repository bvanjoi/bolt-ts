// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/blockScopedFunctionDeclarationES5.ts`, Apache-2.0 License

//@compiler-options: target=ES5

if (true) {
  function foo() { }
  foo();
}
foo();