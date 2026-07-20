// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParameterAsElementType.ts`, Apache-2.0 License

function fee<T>() {
  var t: T;
  var arr = [t, ""];
  //~^ ERROR: Variable 't' is used before being assigned.

  var arr2: (string | T)[] = [t, ""];
  //~^ ERROR: Variable 't' is used before being assigned.
  var arr3: (T | string)[] = [t, ""];
  //~^ ERROR: Variable 't' is used before being assigned.
}
