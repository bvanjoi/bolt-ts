function map(items, f) {
  return items.map(f);
}
function identity(x) {
  return x;
}
function singleton(x) {
  return [x];
}
var xs = [1, 2, 3];
var v1;
var v1 = xs.map(identity);
var v1 = map(xs, identity);
var v2;
var v2 = xs.map(singleton);
var v2 = map(xs, singleton);