var M = {};
(function (M) {

  var x = 3;
  M.x = x
  
  class c {
    fn(M, p = x) {}
  }
  
})(M);

(function (M) {

  class d {
    fn2() {
      var M;
      var p = x;
    }
  }
  
})(M);

(function (M) {

  class e {
    fn3() {
      function M() {
        var p = x;
      }
    }
  }
  
})(M);

(function (M) {

  class f {
    M() {}
  }
  
})(M);