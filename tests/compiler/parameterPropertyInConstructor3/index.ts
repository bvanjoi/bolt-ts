// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterPropertyInConstructor3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
  constructor(public constructor: string) {}
  //~^ ERROR: constructor' cannot be used as a parameter property name.
}