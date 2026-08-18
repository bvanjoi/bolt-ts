// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleWithTryStatement1.ts`, Apache-2.0 License
var M = {};
(function (M) {

  try {} catch (e) {}
  
})(M);
var v = M;