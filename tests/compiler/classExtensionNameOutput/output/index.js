class A {}
if (true) {
  class B extends A {}
  var foo = function () {
    new B();
  };
}
