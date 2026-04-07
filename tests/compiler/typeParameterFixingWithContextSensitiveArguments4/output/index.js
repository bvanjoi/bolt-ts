function f(y, y1, p, p1) {
  return [y, p1(y)]
}
var a, b;
var d = f(a, b, (x) => (x), (x) => (x));