// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileModuleContinuation.ts`, Apache-2.0 License
//@compiler-options: target=es2015

var A = {};
(function (A) {

  var B = {};
  (function (B) {
  
    var C = {};
    (function (C) {
    
      class W {}
      C.W = W;
      
    })(C);
    
  })(B);
  
})(A);