// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/namespaces2.ts`, Apache-2.0 License
var A = {};
(function (A) {

  var B = {};
  (function (B) {
  
    class C {}
    B.C = C;
    
  })(B);
  A.B = B;
  
})(A);
var c = new A.B.C();