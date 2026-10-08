// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceDeclaration1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1 {
    item:number;
    item:number;
    //~^ ERROR: Duplicate identifier 'item'.

}

interface I2 {
    item:any;
    item:number;
    //~^ ERROR: Duplicate identifier 'item'.
    //~| ERROR: Subsequent property declarations must have the same type. Property 'item' must be of type 'any', but here has type 'number'.
}

interface I3 {
    prototype:number;
}

interface I4 {
    class:number;
    number:number;
    super:number;
    prototype:number;
}

interface I5 extends I5 { 
  //~^ ERROR: Type 'I5' recursively references itself as a base type.
    foo():void;
}

interface I6 {
	():void;
}

interface I7 extends I6 { }

var v1:I7;
v1();
//~^ ERROR: Variable 'v1' is used before being assigned.

class C1 implements I3 {
  //~^ ERROR: Property 'prototype' is missing.
    constructor() {
        var prototype: number = 3;
    }
}

interface i8 extends i9 { }
//~^ ERROR: Type 'i8' recursively references itself as a base type.
interface i9 extends i8 { }

interface i10 {
	foo():number;
}

interface i11{
	foo():string;
}

interface i12 extends i10, i11 { }
//~^ ERROR: Interface 'i12' cannot simultaneously extend types 'i10' and 'i11'.