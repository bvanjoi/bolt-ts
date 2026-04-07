class Foo {
  bar() {}
}
class Bar extends Foo {
  x
  constructor() {super();
    this.x = 2;}
  bar() {
    super.bar();
    (super.bar)();
  }
}
var b = new Bar();
b.bar();