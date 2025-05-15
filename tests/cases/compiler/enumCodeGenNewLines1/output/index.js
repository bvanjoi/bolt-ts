// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/emptyTypeArgumentListWithNew.ts`, Apache-2.0 License
var foo = {};
(function (foo) {

  foo[foo['b'] = 1] = 'b'
  foo[foo['c'] = 2] = 'c'
  foo[foo['d'] = 3] = 'd'
})(foo);