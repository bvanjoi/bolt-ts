// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedSelf.ts`, Apache-2.0 License
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