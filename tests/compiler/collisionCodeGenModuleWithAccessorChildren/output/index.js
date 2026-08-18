// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithAccessorChildren.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  var x = 3;
  M.x = x
  
  class c {
    y;
    set Z(M) {
      this.y = x;
    }
  }
  
})(M);

(function (M) {

  class d {
    y;
    set Z(p) {
      var M = 10;
      this.y = x;
    }
  }
  
})(M);

(function (M) {

  class e {
    y;
    set M(p) {
      this.y = x;
    }
  }
  
})(M);

(function (M) {

  class f {
    get Z() {
      var M = 10;
      return x;
    }
  }
  
})(M);

(function (M) {

  class e {
    get M() {
      return x;
    }
  }
  
})(M);