// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleSharesNameWithImportDeclarationInsideIt2.ts`, Apache-2.0 License
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
  Z// Should call Z.M.bar
  .M = M;
  
})(Z);
// Should call Z.M.bar
var A = {};
(function (A) {

  var M = {};
  (function (M) {
  
    var M = Z.M
    
    function bar() {}
    M.bar = bar;
    
    M.bar();
    
    var a = M.bar();
    
  })(M);
  A.M = M;
  
})(A);