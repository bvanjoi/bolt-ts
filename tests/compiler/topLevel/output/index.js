// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/topLevel.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class Point {
  constructor(x, y) {
    this.x = x
    
    this.y = y}
  move(xo, yo) {
    this.x += xo;
    this.y += yo;
    return this;
  }
  toString() {
    return ('(' + this.x + ',' + this.y + ')');
  }
}
var result = '';
result += (new Point(3, 4).move(2, 2));
var M = {};
(function (M) {

  var origin = new Point(0, 0);
  M.origin = origin
  
})(M);
result += (M.origin.move(1, 1));