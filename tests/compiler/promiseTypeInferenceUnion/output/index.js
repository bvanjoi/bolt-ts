// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseTypeInferenceUnion.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: target=esnext
function f1(x) {
  return Promise.resolve(x);
}
function f2(x) {
  return Promise.resolve(x);
}
function f3(x) {
  return Promise.resolve(x);
}
var g1 = Promise.resolve(f1(42));
var g2 = Promise.resolve(f2(42));
var g3 = Promise.resolve(f3(42));