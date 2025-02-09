// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeParameterAndArgumentOfSameName1.ts`, Apache-2.0 License

function f<A extends Number>(A: A): A {
  var r = A.toExponential(123);
  return null;
}