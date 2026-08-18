// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictNullLogicalAndOr.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var sinOrCos = Math.random() < 0.5;
var choice = sinOrCos && Math.sin || Math.cos;
choice(Math.PI);
function sq(n) {
  var r = n !== undefined && n * n || 0;
  return r;
}
sq(3);