// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instanceAndStaticDeclarations1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Point {
  constructor(x, y) {
    this.x = x
    
    this.y = y}
  distance(p) {
    var dx = this.x - p.x;
    var dy = this.y - p.y;
    return Math.sqrt(dx * dx + dy * dy);
  }
  static origin = new Point(0, 0);
  static distance(p1, p2) {
    return p1.distance(p2);
  }
}