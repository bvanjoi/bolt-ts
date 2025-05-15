// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/newArrays.ts`, Apache-2.0 License
var M = {};
(function (M) {

  class Foo {}
  
  class Gar {
    fa
    x = 10
    y = 10
    m() {
      this.fa = new Array(this.x * this.y);
    }
  }
  
})(M);