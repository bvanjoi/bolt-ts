// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ParameterList13.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
    new (public x);
    //~^ ERROR: Construct signature, which lacks return-type annotation, implicitly has an 'any' return type.
    //~| ERROR: A parameter property is only allowed in a constructor implementation.
    //~| ERROR: Parameter 'x' implicitly has an 'any' type.
}