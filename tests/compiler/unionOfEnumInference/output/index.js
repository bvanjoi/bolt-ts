var Enum = {};
(function (Enum) {

  Enum[Enum['A'] = 0] = 'A'
  Enum[Enum['B'] = 0] = 'B'
  Enum[Enum['C'] = 0] = 'C'
})(Enum);
function foo(x) {}
function bar(x) {
  foo(x);
}