function f1(x) {
  return x
}
class C {
  constructor() {var bar = (function () {
      return bar
    });
    var b = f1(f1(bar));}
}
class Vector {
  static norm(v) {
    return null
  }
  static minus(v1, v2) {
    return null
  }
  static times(v1, v2) {
    return null
  }
  static cross(v1, v2) {
    return null
  }
  constructor(x, y, z) {
    this.x = x
    
    this.y = y
    
    this.z = z}
  static dot(v1, v2) {
    return null
  }
}
class Camera {
  forward;
  right;
  up;
  constructor(pos, lookAt) {
    var down = new Vector(0, -1, 0);
    this.forward = Vector.norm(Vector.minus(lookAt, this.pos));
    this.right = Vector.times(down, Vector.norm(Vector.cross(this.forward, down)));
    this.up = Vector.times(down, Vector.norm(Vector.cross(this.forward, this.right)));
    this.pos = pos
    
    }
}