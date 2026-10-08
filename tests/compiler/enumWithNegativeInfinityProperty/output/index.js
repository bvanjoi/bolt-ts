// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumWithNegativeInfinityProperty.ts`, Apache-2.0 License
var A = {};
(function (A) {

  A[A['-Infinity'] = 1] = '-Infinity'
})(A);