var M1 = {};
(function (M1) {

  function reduce(ar, f, e) {
    return Array.prototype.reduce.apply(ar, e ? [f, e] : [f])
  }
  M1.reduce = reduce;
  
  ;
  
})(M1);
;
var M2 = {};
(function (M2) {

  var A = M1
  
  function compose() {
    A.reduce(arguments, compose2);
  }
  M2.compose = compose;
  
  ;
  
  function compose2(g, f) {
    return function (x) {
      return g(f(x))
    }
  }
  M2.compose2 = compose2;
  
  ;
  
})(M2);
;