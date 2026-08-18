// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ClassTest3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  class Visibility {
    foo() {}
    bar() {}
    x;
    y;
    z;
    constructor() {this.x = 1;
      this.y = 2;}
  }
  
})(M);