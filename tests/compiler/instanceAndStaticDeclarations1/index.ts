// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instanceAndStaticDeclarations1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// from spec

class Point {
    constructor(public x: number, public y: number) { }
    public distance(p: Point) {
        var dx = this.x - p.x;
        var dy = this.y - p.y;
        return Math.sqrt(dx * dx + dy * dy);
    }
    static origin = new Point(0, 0);
    static distance(p1: Point, p2: Point) { return p1.distance(p2); }
}