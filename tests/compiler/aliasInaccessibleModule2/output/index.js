// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/aliasInaccessibleModule2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  var N = {};
  (function (N) {
  
    class C {}
    
  })(N);
  
  var R = N
  
  var X = R
  
})(M);