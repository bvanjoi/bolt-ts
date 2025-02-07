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