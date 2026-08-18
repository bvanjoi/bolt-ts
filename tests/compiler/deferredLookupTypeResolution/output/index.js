// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deferredLookupTypeResolution.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
function f2(a) {
  return f1(a, 'x');
}
function f3(x) {
  return f2(x);
}