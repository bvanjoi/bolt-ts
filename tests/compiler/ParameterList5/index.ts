// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ParameterList5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function A(): (public B) => C {
  //~^ ERROR: A parameter property is only allowed in a constructor implementation.
  //~| ERROR: Cannot find name 'C'.
  //~| ERROR: Parameter 'B' implicitly has an 'any' type.
  //~| ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
}