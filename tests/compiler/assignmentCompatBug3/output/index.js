function makePoint(x, y) {
  return {
      get x() {
      return x
    },
    get y() {
      return y
    },
    dist: function () {
      return Math.sqrt(x * x + y * y)
    }    
  }
}
class C {
  get x() {
    return 0
  }
}
function foo(test) {}
var x;
var y;
foo(x);
foo(x + y);