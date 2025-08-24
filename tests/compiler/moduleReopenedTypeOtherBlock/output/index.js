var M = {};
(function (M) {

  class C1 {}
  M.C1 = C1;
  
})(M);

(function (M) {

  class C2 {
    f() {
      return null
    }
  }
  M.C2 = C2;
  
})(M);