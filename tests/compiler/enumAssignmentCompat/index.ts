// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumAssignmentCompat.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace W {
    export class D { }
}

enum W {

    a, b, c,

}


interface WStatic {

    a: W;

    b: W;

    c: W;

}



var x: WStatic = W;
var y: typeof W = W;
var z: number = W; // error
//~^ ERROR: Type 'typeof W' is not assignable to type 'number'.
var a: number = W.a;
var b: typeof W = W.a; // error
//~^ ERROR: Type 'W' is not assignable to type 'typeof W'.
var c: typeof W.a = W.a;
var d: typeof W = 3; // error
//~^ ERROR: Type 'number' is not assignable to type 'typeof W'.
var e: typeof W.a = 4;
//~^ ERROR: Type '4' is not assignable to type 'W.a'.
var f: WStatic = W.a; // error
//~^ ERROR: Type 'W' is not assignable to type 'WStatic'.
var g: WStatic = 5; // error
//~^ ERROR: Type 'number' is not assignable to type 'WStatic'.
var h: W = 3;
//~^ ERROR: Type 'number' is not assignable to type 'W'.
var i: W = W.a;
i = W.a;
W.D;
var p: W.D;