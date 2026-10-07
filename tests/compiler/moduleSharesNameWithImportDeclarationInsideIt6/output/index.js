// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleSharesNameWithImportDeclarationInsideIt6.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Z = {};
(function (Z) {

  var M = {};
  (function (M) {
  
    function bar() {
      return '';
    }
    M.bar = bar;
    
  })(M);
  Z.M = M;
  
})(Z);
var A = {};
(function (A) {

  var M = {};
  (function (M) {
  
    var M = Z.M
    
    function bar() {}
    M.bar = bar;
    
  })(M);
  A.M = M;
  
})(A);