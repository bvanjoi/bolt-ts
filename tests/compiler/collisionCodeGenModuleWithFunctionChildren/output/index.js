var M = {};
(function (M) {

  var x = 3;
  M.x = x
  
  function fn(M, p = x) {}
  
})(M);

(function (M) {

  function fn2() {
    var M;
    var p = x;
  }
  
})(M);

(function (M) {

  function fn3() {
    function M() {
      var p = x;
    }
  }
  
})(M);