// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/capturedLetConstInLoop10.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  foo() {
    for ( var x of [0]) {
      var f = function () {
        return x;
      };
      this.bar(f());
    }
  }
  bar(a) {}
  baz() {
    for ( var x of [1]) {
      var a = function () {
        return x;
      };
      for ( var y of [1]) {
        var b = function () {
          return y;
        };
        this.bar(b());
      }
      this.bar(a());
    }
  }
  baz2() {
    for ( var x of [1]) {
      var a = function () {
        return x;
      };
      this.bar(a());
      for ( var y of [1]) {
        var b = function () {
          return y;
        };
        this.bar(b());
      }
    }
  }
}
class B {
  foo() {
    var a = () => {
      for ( var x of [0]) {
        var f = () => (x);
        this.bar(f());
      }
    };
  }
  bar(a) {}
}