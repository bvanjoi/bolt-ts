// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superAccessCastedCall.ts`, Apache-2.0 License
class Foo {
  bar() {}
}
class Bar extends Foo {
  x;
  constructor() {super();this.x = 2;}
  bar() {
    super.bar();
    (super.bar)();
  }
}
var b = new Bar();
b.bar();