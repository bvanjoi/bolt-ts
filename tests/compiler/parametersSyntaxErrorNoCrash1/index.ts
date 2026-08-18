// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parametersSyntaxErrorNoCrash1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit


// https://github.com/microsoft/TypeScript/issues/59422

function identity<T>(arg: T: T {
  //~^ ERROR: Expected ','.
  //~| ERROR: Expected ','.
  //~| ERROR: Function implementation is missing or not immediately following the declaration.
  //~| ERROR: Parameter 'T' implicitly has an 'any' type.
  //~| ERROR: 'identity', which lacks return-type annotation, implicitly has an 'any' return type.
    return arg;
  //~^ ERROR: Expected ':'.
  //~| ERROR: Expected ','.
  //~| ERROR: Duplicate identifier 'arg'.
  //~| ERROR: 'arg' is an unused renaming of 'return'. Did you intend to use it as a type annotation?
} //~ ERROR: Expected ')'.