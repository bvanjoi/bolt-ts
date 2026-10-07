// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumLiteralUnionNotWidened.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var A = {};
(function (A) {

  A[A['one'] = 'one'] = 'one'
  A[A['two'] = 'two'] = 'two'
})(A);
;
var B = {};
(function (B) {

  B[B['foo'] = 'foo'] = 'foo'
  B[B['bar'] = 'bar'] = 'bar'
})(B);
;
class // TypeScript incorrectly infers the return type of "asList(x)" to be "List<A | B>"
// The correct type is "List<A | B.foo>"
List {
  items = [];
}
function asList(arg) {
  return new List();
// If we use the literal "foo" instead of B.foo, the correct type is inferred
}
function fn1(x) {
  return asList(x);
}
function fn2(x) {
  return asList(x);
}