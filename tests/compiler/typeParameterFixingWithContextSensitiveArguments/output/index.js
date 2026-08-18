// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterFixingWithContextSensitiveArguments.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function f(y, f, x) {
  return [y, f(x)];
}
var a, b;
var d = f(b, (x) => (x.a), a);
var d2 = f(b, (x) => (x.a), null);
var d3 = f(b, (x) => (x.b), null);