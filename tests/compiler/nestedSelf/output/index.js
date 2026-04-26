var M = {};
(function (M) {

  class C {
    n = 42;
    foo() {
      [1, 2, 3].map((x) => (this.n * x));
    }
  }
  M.C = C;
  
})(M);