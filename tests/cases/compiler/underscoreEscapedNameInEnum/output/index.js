// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/underscoreEscapedNameInEnum.ts`, Apache-2.0 License
var E = {};
(function (E) {

  E[E['__foo'] = 1] = '__foo'
  E[E['bar'] = E["__foo"] + 1] = 'bar'
})(E);
var a0 = E.__foo;
var a1 = E.bar;
var a2 = E["__foo"];
var a3 = E["bar"];