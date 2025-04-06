// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParametersShouldNotBeEqual.ts`, Apache-2.0 License

function ff<T, U>(x: T, y: U) {
  var z: Object;
  x = x;  // Ok
  x = y;  // Error
  //~^ ERROR: Type 'U' is not assignable to type 'T'.
  x = z;  // Error
  //~^ ERROR: Type 'Object' is not assignable to type 'T'.
  z = x;  // Ok
}
