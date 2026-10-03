
var A = {};
(function (A) {

  var B = {};
  (function (B) {
  
    var C = {};
    (function (C) {
    
      class W {}
      C.W = W;
      
    })(C);
    B.C = C;
    
  })(B);
  A.B = B;
  
})(A);