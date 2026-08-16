// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationConstructorOverloadSyntax.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs
//@compiler-options: noEmit

class A {
  constructor();
  //~^ ERROR: Signature declarations can only be used in TypeScript files.
  //~| ERROR: Constructor implementation is missing.
}

1 << 5