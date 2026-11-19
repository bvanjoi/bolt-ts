// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/blockScopedFunctionDeclarationES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

if (true) {
  function foo() { }
  //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
  foo();
}
foo();
//~^ ERROR: Cannot find name 'foo'.
