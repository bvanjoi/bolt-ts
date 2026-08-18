// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterFixingWithContextSensitiveArguments5.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f(t1, u1, pf1, pf2) {
  return [t1, pf2(t1)];
}
var a, b;
var d = f(a, b, (u2) => (u2.b), (t2) => (t2));