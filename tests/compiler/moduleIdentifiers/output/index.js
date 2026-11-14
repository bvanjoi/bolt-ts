// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleIdentifiers.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  var a = 1;
  M.a //var p: M.P;
  //var m: M = M;
  = a
  
})(M);
var x1 = M.a;