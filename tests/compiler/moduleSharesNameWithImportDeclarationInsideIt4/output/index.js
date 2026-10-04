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
    
    M.bar();
    
    var a = M.bar();
    
  })(M);
  A.M = M;
  
})(A);