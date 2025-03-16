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