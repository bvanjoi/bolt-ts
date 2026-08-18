// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithFunctionChildren.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  var x = 3;
  M.x = x
  
  function fn(M, p = x) {}
  
})(M);

(function (M) {

  function fn2() {
    var M;
    var p = x;
  }
  
})(M);

(function (M) {

  function fn3() {
    function M() {
      var p = x;
    }
  }
  
})(M);