var _super = 10;
class Foo {
  get prop1() {
    var _super = 10;
    return 10;
  }
  set prop1(val) {
    var _super = 10;
  }
}
class b extends Foo {
  get prop2() {
    var _super = 10;
    return 10;
  }
  set prop2(val) {
    var _super = 10;
  }
}
class c extends Foo {
  get prop2() {
    var x = () => {
      var _super = 10;
    };
    return 10;
  }
  set prop2(val) {
    var x = () => {
      var _super = 10;
    };
  }
}