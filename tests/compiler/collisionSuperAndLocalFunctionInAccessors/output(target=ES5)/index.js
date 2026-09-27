function _super() {}
class Foo {
  get prop1() {
    function _super() {}
    return 10;
  }
  set prop1(val) {
    function _super() {}
  }
}
class b extends Foo {
  get prop2() {
    function _super() {}
    return 10;
  }
  set prop2(val) {
    function _super() {}
  }
}
class c extends Foo {
  get prop2() {
    var x = () => {
      function _super() {}
    };
    return 10;
  }
  set prop2(val) {
    var x = () => {
      function _super() {}
    };
  }
}