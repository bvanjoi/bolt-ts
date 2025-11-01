var A = {};
(function (A) {

  var B = {};
  (function (B) {
  
    function createB() {
      return null
    }
    B.createB = createB;
    
  })(B);
  A.B = B;
  
})(A);
var x = A.B.createB();