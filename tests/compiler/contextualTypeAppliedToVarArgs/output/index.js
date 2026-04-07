function delegate(instance, method, data) {
  return function () {}
}
class Foo {
  Bar() {
    delegate(this, function (source, args2) {
      var a = source.node;
      var b = args2.node;
    });
  }
}