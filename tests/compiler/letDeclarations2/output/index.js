// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letDeclarations2.ts`, Apache-2.0 License
//@compiler-options: target=ES6
var M = {};
(function (M) {

  var l1 = 's';
  
  var l2 = 0;
  M.l2 = l2
  
})(M);