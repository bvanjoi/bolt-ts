// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorOverloads7.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare class Point
{
    x: number;
    y: number;
    constructor(x: number, y: number);

     add(dx: number, dy: number): Point;
     origin: Point;

}

// Type provided by extern declaration
// Because Point is a constructor function, this is inferred
// to be Point and return type is inferred to be void
function Point(x, y) {
    this.x = x;
    this.y = y;

    return this;
}

declare function EF1(a:number, b:number):number;
//~^ ERROR: Overload signatures must all be ambient or non-ambient.

function EF1(a,b) { return a+b; }
