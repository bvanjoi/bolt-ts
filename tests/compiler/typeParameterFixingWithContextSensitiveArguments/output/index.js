function f(y, f, x) {
  return [y, f(x)]
}
var a, b;
var d = f(b, (x) => (x.a), a);
var d2 = f(b, (x) => (x.a), null);
var d3 = f(b, (x) => (x.b), null);