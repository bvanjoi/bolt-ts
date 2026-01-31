class C {
  static foo(x) {}
  type
}
var M = {};
(function (M) {

  function f(x) {
    return new x()
  }
  M.f = f;
  
})(M);