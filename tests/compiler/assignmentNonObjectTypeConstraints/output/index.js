var E = {};
(function (E) {

  E[E['A'] = 0] = 'A'
  E[E['B'] = 0] = 'B'
  E[E['C'] = 0] = 'C'
})(E);
function foo(x) {
  var y = x;
}
foo(5);
foo(E.A);
class A {
  a;
}
class B {
  b;
}
function bar(x) {
  var y = x;
}
bar(new A());
bar(new B());