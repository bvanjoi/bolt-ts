

fn(a);
var E = {};
(function (E) {

  E[E['A'] = 'foo'] = 'A'
  E[E['B'] = 'bar'] = 'B'
})(E);
var x = enumValues(E);