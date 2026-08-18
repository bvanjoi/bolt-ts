// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/higherOrderMappedIndexLookupInference.ts`, Apache-2.0 License
//@compiler-options: strict
function f1(a, b) {
  a = b;
  b = a;
}
function f2(a, b) {
  a = b;
  b = a;
}
function f3(a, b) {
  a = b;
  b = a;
}


var h = f;
var h1 = g;