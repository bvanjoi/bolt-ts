// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentNonObjectTypeConstraints.ts`, Apache-2.0 License
//@compiler-options: target=es2015
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