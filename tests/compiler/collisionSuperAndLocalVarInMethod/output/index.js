// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalVarInMethod.ts`, Apache-2.0 License
var _super = 10;
class Foo {
  x() {
    var _super = 10;
  }
}
class b extends Foo {
  foo() {
    var _super = 10;
  }
}
class c extends Foo {
  foo() {
    var x = () => {
      var _super = 10;
    };
  }
}