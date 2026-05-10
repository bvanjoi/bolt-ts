class a {
  foo() {}
}
class b extends a {
  foo() {
    var _this = 10;
    var f = () => (super.foo());
  }
}
class b2 extends a {
  foo() {
    var f = () => {
      var _this = 10;
      return super.foo()
    };
  }
}