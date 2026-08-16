// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyDeclareTypePropertyWithoutType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

class C {
    constructor() { }
}

// this should be an error
var x: { y; z; }             // error at "y,z"
//~^ ERROR: Member 'y' implicitly has an 'any' type.
//~| ERROR: Member 'z' implicitly has an 'any' type.
var x1: { y1: C; z1; };      // error at "z1" 
//~^ ERROR: Member 'z1' implicitly has an 'any' type.
var x11: { new (); };        // error at "new"
//~^ ERROR: Construct signature, which lacks return-type annotation, implicitly has an 'any' return type.
var x2: (y2) => number;      // error at "y2"
//~^ ERROR: Parameter 'y2' implicitly has an 'any' type.
var x3: (x3: string, y3) => void ; // error at "y3"
//~^ ERROR: Parameter 'y3' implicitly has an 'any' type.

// this should not be an error
var bar: { a: number; b: number };
var foo: { littleC: C; c: string };
var x4: new () => any;
var x5: () => any;
