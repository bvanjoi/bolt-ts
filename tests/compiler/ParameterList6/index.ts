// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ParameterList6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
  constructor(C: (public A) => any) {
    //~^ ERROR: A parameter property is only allowed in a constructor implementation.
  }
}
