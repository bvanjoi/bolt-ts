// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumWithUnicodeEscape1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var E = {};
(function (E) {

  E[E['gold \u2730'] = 0] = 'gold \u2730'
})(E);