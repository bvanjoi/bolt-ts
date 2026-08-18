// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations2.ts`, Apache-2.0 License
//@compiler-options: target=es6
//@compiler-options: strict=false
//@compiler-options: declaration
var M = {};
(function (M) {

  var c1 = false;
  M.c1 = c1
  
  var c2 = 23;
  M.c2 = c2
  
  var c3 = 0, c4 = '', c5 = null;
  M.c3 = c3
  M.c4 = c4
  M.c5 = c5
  
})(M);