var M = {};
(function (M) {

  class C {}
  M.C = C;
  
  class D extends M.C {}
  
})(M);