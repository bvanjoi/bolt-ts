// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationMethodOverloadSyntax.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs
//@compiler-options: noEmit

class A {
  foo();
  //~^ ERROR: Signature declarations can only be used in TypeScript files.
  //~| ERROR: Function implementation is missing or not immediately following the declaration.
  //~| ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
}
