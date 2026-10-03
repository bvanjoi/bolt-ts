var M = {};
(function (M) {

  var x = 1;
  M.x = x
  
  var N = {};
  (function (N) {
  
    var y = 2;
    N.y = y
    
  })(N);
  M.N = N;
  
})(M);
var A = {};
(function (A) {

  var N = M.N
  
  var r = N.y;
  
  var r2 = M.N.y;
  
})(A);