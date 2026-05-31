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