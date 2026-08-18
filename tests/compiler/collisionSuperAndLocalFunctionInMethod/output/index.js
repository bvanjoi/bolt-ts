// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalFunctionInMethod.ts`, Apache-2.0 License
function _super() {}
class Foo {
  x() {
    function _super() {}
  }
  _super() {}
}
class b extends Foo {
  foo() {
    function _super() {}
  }
  _super() {}
}
class c extends Foo {
  foo() {
    var x = () => {
      function _super() {}
    };
  }
  _super() {}
}