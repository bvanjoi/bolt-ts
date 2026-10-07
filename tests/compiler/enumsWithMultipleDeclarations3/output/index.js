
// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumsWithMultipleDeclarations3.ts`, Apache-2.0 License
var E = {};
(function (E) {

  E[E['A'] = 0] = 'A'
})(E);