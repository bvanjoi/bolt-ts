// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithConstructorChildren.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  var x = 3;
  M.x = x
  
  class c {
    constructor(M, p = x) {}
  }
  
})(M);

(function (M) {

  class d {
    constructor(M, p = x) {}
  }
  
})(M);

(function (M) {

  class d2 {
    constructor() {var M = 10;
      var p = x;}
  }
  
})(M);