// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeParameterAsElementType.ts`, Apache-2.0 License

function fee<T>() {
  var t: T;
  var arr = [t, ""];

  var arr2: (string | T)[] = [t, ""];
  var arr3: (T | string)[] = [t, ""];
}