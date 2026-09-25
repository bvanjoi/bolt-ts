export {  }
class Bar {
  num;
  Value = callme(new Foo(this)).bar.num;
}
class Foo {
  bar;
  constructor(bar) {this.bar = bar;}
}