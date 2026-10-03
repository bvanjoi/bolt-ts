var WithLocation = (Base) => (class extends Base {
  getLocation() {
    var [x, y] = super.getLocation();
    return [this.x | x, this.y | y];
  }
});
class Point {
  constructor(x, y) {
    this.x = x
    
    this.y = y}
  getLocation() {
    return [0, 0];
  }
}
class Foo extends WithLocation(Point) {
  calculate() {
    return this.x + this.y;
  }
  getLocation() {
    return super.getLocation();
  }
  whereAmI() {
    return this.getLocation();
  }
}