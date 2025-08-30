// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/typeArgumentsOnFunctionsWithNoTypeParameters.ts`, Apache-2.0 License

function foo<T, U>(f: (v: T) => U) {
  var r1 = f<number>(1);
  //~^ ERROR: Expected 0 type arguments, but got 1.
  var r2 = f(1);
  //~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'T'.
  var r3 = f<any>(null);
  //~^ ERROR: Expected 0 type arguments, but got 1.
  var r4 = f(null);
}
