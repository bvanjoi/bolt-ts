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