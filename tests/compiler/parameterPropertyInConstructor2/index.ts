// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterPropertyInConstructor2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace mod {
  class Customers {
    constructor(public names: string);
    //~^ ERROR: A parameter property is only allowed in a constructor implementation.
    //~| ERROR: This overload signature is not compatible with its implementation signature.
    constructor(public names: string, public ages: number) {
      //~^ ERROR: Duplicate identifier 'names'.
    }
  }
}

