// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClasses4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Vec2_T {
  constructor(x, y) {
    this.x = x
    
    this.y = y}
  fmap(f) {
    var x = f(this.x);
    var y = f(this.y);
    var retval = new Vec2_T(x, y);
    return retval;
  }
  apply(f) {
    var x = f.x(this.x);
    var y = f.y(this.y);
    var retval = new Vec2_T(x, y);
    return retval;
  }
}