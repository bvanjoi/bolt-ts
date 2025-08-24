var M = {};
(function (M) {

  class C1 {
    value
  }
  M.C1 = C1;
  
  class A {
    dummy
  }
  M.A = A;
  
  class B extends C1 {}
  M.B = B;
  
  class D {
    _subscribe(viewModel) {
      var f = (newValue) => {};
      var v = viewModel.value;
      v.subscribe(f);
      v.subscribe((newValue) => {});
    }
  }
  M.D = D;
  
})(M);