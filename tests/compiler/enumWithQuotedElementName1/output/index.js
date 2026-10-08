// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/enumWithQuotedElementName1.ts`, Apache-2.0 License
var E = {};
(function (E) {

  E[E['fo"o'] = 0] = 'fo"o'
})(E);