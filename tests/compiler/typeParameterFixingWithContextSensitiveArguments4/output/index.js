// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterFixingWithContextSensitiveArguments4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f(y, y1, p, p1) {
  return [y, p1(y)];
}
var a, b;
var d = f(a, b, (x) => (x), (x) => (x));