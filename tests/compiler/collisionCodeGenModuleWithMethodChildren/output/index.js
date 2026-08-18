// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithMethodChildren.ts`, Apache-2.0 License
//@compiler-options: target=esnext
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