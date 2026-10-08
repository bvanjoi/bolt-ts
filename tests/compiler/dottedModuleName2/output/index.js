// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/dottedModuleName2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var A = {};
(function (A) {

  var B = {};
  (function (B) {
  
    var x = 1;
    B.x = x
    
  })(B);
  A.B = B;
  
})(A);
var AA = {};
(function (AA) {

  var B = {};
  (function (B) {
  
    var x = 1;
    B.x = x
    
  })(B);
  AA.B = B;
  
})(AA);
var tmpOK = AA.B.x;
var tmpError = A.B.x;

(function (A) {

  var B = {};
  (function (B) {
  
    var C = {};
    (function (C) {
    
      var x = 1;
      C.x = x
      
    })(C);
    B.C = C;
    
  })(B);
  A.B = B;
  
})(A);
