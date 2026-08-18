// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleIdentifiers.ts`, Apache-2.0 License
var M = {};
(function (M) {

  var a = 1;
  M.a = a
  
})(M);
var x1 = M.a;