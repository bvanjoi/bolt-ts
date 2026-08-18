// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/divergentAccessorsTypes1.ts`, Apache-2.0 License
//@compiler-options: target=esnext
class Test1 {
  get foo() {
    return '';
  }
  set foo(s) {
    var a = s;
    var b = s;
  }
  get bar() {
    return '';
  }
  set bar(s) {}
}
{
  var t = new Test1();
  t.foo = 32;
  var m = t.foo;
  t.bar = 42;
  var n = t.bar;
  t.bar = false;
  var o = t.bar;
}
{
  var t = {};
  t.foo = 32;
  var m = t.foo;
  t.bar = 42;
  var n = t.bar;
  t.bar = false;
  var o = t.bar;
}
{
  var t = {};
  t.foo = 32;
  var m = t.foo;
  t.bar = 42;
  var n = t.bar;
  t.bar = false;
  var o = t.bar;
}