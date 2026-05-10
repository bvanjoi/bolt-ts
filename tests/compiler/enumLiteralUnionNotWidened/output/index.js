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
class List {
  items = [];
}
function asList(arg) {
  return new List()
}
function fn1(x) {
  return asList(x)
}
function fn2(x) {
  return asList(x)
}