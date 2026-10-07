// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/superAccessInFatArrow1.ts`, Apache-2.0 License
var test = {};
(function (test) {

  class A {
    foo() {}
  }
  test.A = A;
  
  class B extends A {
    bar(callback) {}
    runme() {
      this.bar(() => {
        super.foo();
      });
    }
  }
  test.B = B;
  
})(test);