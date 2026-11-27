
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleVariables.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: lib=[es5]
var x = 1;
var M = {};
(function (M) {

  var x = 2;
  M.x = x
  
  console.// 2
  log(x);
  
})(M);// 2


(function (M) {

  console.log(x);
  
})(M// 3
);

(function (M) {

  var x = 3;
  
  console.log(x);
  
})(M);