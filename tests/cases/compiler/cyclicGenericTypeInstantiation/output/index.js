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
a = b;