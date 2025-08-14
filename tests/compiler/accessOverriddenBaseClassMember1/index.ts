// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/accessOverriddenBaseClassMember1.ts`, Apache-2.0 License

class Point {
  constructor(public x: number, public y: number) { }
  public toString() {
      return "x=" + this.x + " y=" + this.y;
  }
}
class ColoredPoint extends Point {
  constructor(x: number, y: number, public color: string) {
      super(x, y);
  }
  public toString() {
      return super.toString() + " color=" + this.color;
  }
}

let p = new Point(1, 2);
let px: number = p.x;
let py: number = p.y;

let cp = new ColoredPoint(1, 2, "red");
let cpColor: string = cp.color;