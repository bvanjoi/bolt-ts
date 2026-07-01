var console;
class Foo {
  x() {
    var _this = 10;
    function inner() {
      console.log(_this);
      return (x) => (this);
    }
  }
}