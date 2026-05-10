function f(x) {
  return x
}
function g(x) {
  return x
}
var E = {};
(function (E) {

  E[E['A'] = 0] = 'A'
  E[E['B'] = 0] = 'B'
  E[E['C'] = 0] = 'C'
  E[E['non identifier'] = 0] = 'non identifier'
})(E);
var c1 = 'abc';
var c2 = 123;
var c3 = c1;
var c4 = c2;
var c5 = f(123);
var c6 = f(-123);
var c7 = true;
var c8 = E.A;
var c8b = E['non identifier'];
var c9 = {
  x: 'abc'  
};
var c10 = [123];
var c11 = 'abc' + 'def';
var c12 = 579;
var c13 = Math.random() > 0.5 ? 'abc' : 'def';
var c14 = Math.random() > 0.5 ? 123 : 456;