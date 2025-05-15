// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cyclicGenericTypeInstantiationInference.ts`, Apache-2.0 License
//@ run-fail
function foo() {
  var z = foo();
  var y;
  return y
}
function bar() {
  var z = bar();
  var y;
  return y
}
var a = foo();
var b = bar();
a.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2;
b.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2;
function test(x) {}
test(b);