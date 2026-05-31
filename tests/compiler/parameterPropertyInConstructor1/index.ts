// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterPropertyInConstructor1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace mod {
  class Customers {
    constructor(public names: string);
    //~^ ERROR: A parameter property is only allowed in a constructor implementation.
  }
}