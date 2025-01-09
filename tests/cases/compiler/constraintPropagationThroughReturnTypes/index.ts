// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/constraintPropagationThroughReturnTypes.ts`, Apache-2.0 License

function g<T>(x: T): T {
  return x;
}
 
function f<S extends { foo: string }>(x: S) {
  var y = g(x);
  y;
}
