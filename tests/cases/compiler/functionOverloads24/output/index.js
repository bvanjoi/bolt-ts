

function foo(bar) {
  return function () {}
}
var x = foo(5);
var x0 = x;
var y = foo("");
var y0 = y;