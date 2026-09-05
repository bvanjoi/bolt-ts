function update(b) {
  var c = copy(b);
  add(c, c);
}
function add(a, b, c = a) {
  c[0] = a[0] + b[0];
}
function copy(a) {
  return new Float32Array(a);
}