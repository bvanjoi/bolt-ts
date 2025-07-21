class Point {
  constructor(x, y) {
    this.x = x
    
    this.y = y}
  toString() {
    return 'x=' + this.x + ' y=' + this.y
  }
}
class ColoredPoint extends Point {
  constructor(x, y, color) {
    super(x, y);
    this.color = color
    }
  toString() {
    return super.toString() + ' color=' + this.color
  }
}
var p = new Point(1, 2);
var px = p.x;
var py = p.y;
var cp = new ColoredPoint(1, 2, 'red');
var cpColor = cp.color;