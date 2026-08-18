// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyAnyReturningFunction.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function A() {
  return '';
}
function B() {
  var someLocal = {};
  return someLocal;
}
class C {
  A() {
    return '';
  }
  B() {
    var someLocal = {};
    return someLocal;
  }
}