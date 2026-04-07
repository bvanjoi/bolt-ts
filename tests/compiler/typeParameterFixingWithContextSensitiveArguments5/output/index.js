function f(t1, u1, pf1, pf2) {
  return [t1, pf2(t1)]
}
var a, b;
var d = f(a, b, (u2) => (u2.b), (t2) => (t2));