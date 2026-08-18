// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferringAnyFunctionType5.ts`, Apache-2.0 License
function f(p) {
  return p;
}
var v = f({
  q: (x) => (x)  
});