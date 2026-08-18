// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorOverloads2.ts`, Apache-2.0 License
class FooBase {
  constructor(x) {}
  bar1() {}
}
class Foo extends FooBase {
  constructor(x, y) {super(x);}
  bar1() {}
}
var f1 = new Foo('hey');
var f2 = new Foo(0);
var f3 = new Foo(f1);
var f4 = new Foo([f1, f2, f3]);
f1.bar1();