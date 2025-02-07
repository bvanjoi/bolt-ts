function f(p) {
  return p
}
var v = f({q: (x) => x});
var v2 = f({q: (x) => x + 1}).q(1);
var v3 = f({q: (x) => x + 1}).q(1);