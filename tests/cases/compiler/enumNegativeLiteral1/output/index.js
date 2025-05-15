// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/enumNegativeLiteral1.ts`, Apache-2.0 License
var E = {};
(function (E) {

  E[E['a'] = -5] = 'a'
  E[E['b'] = 0] = 'b'
  E[E['c'] = 0] = 'c'
})(E);