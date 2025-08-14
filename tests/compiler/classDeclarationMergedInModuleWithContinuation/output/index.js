var M = {};
(function (M) {

  class N {}
  M.N = N;
  
  
  (function (N) {
  
    var v = 0;
    N.v = v
    
  })(N);
  M.N = N;
  
})(M);

(function (M) {

  class O extends M.N {}
  M.O = O;
  
})(M);
var a0 = new M.N();
var a1 = M.N.v;
var a2 = new M.O();