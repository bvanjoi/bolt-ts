var A = {};
(function (A) {

  class B {
    foo() {}
    static bar() {}
  }
  A.B = B;
  
})(A);

(function (A) {

  var B = {};
  (function (B) {
  
    var x = 1
    B.x = x
    
  })(B);
  A.B = B;
  
})(A);
var b