// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anyAsReturnTypeForNewOnCall.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function Point(x, y) {
  this.x = x;
  this.y = y;
}
var o = new Point(3, 4);
var xx = o.x;