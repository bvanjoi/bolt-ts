// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumWithNaNProperty.ts`, Apache-2.0 License
var A = {};
(function (A) {

  A[A['NaN'] = 1] = 'NaN'
})(A);