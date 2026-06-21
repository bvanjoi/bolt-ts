// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ClassTest4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare class Point
{
    x: number;
    y: number;
    add(dx: number, dy: number): Point;
    mult(p: Point): Point;
    static origin: Point;
    constructor(x: number, y: number);
}
