class P {
  x = this
  static y = this
  constructor(z = this, zz = this, zzz = (p = this) => this) {
    zzz = (p = this) => this;
    zz.x;
    this.z = z
    
    }
  foo(zz = this) {
    zz.x;
    this.x;
  }
  static bar(zz = this) {
    zz.y;
  }
}