var M = {};
(function (M) {

  class N {}
  M.N = N;
  
  
  (function (N) {
  
    var v = 0
    N.v = v
    
  })(N);
  M.N = N;
  
})(M);

(function (M) {

  class O extends M.N {}
  M.O = O;
  
})(M);