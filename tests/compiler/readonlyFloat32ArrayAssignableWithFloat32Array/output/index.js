// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/readonlyFloat32ArrayAssignableWithFloat32Array.ts`, Apache-2.0 License
//@compiler-options: target=es2015
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