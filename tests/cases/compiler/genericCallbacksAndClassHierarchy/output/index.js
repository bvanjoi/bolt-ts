// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericCallbacksAndClassHierarchy.ts`, Apache-2.0 License
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
      // both of these should work
      v.subscribe(f);
      v.subscribe((newValue) => {});
    }
  }
  M.D = D;
  
})(M);