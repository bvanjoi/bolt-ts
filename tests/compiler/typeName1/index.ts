// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeName1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
    k;
}
class C {
    public eek:string;
    static zeek:number;
}

var x1:{ f(s:string):number;f(n:number):string; }=3;
//~^ ERROR: Type 'number' is not assignable to type '{ f: (s: string) => number; }'.
var x2:{ f(s:string):number; } =3;
//~^ ERROR: Type 'number' is not assignable to type '{ f: (s: string) => number; }'.
var x3:{ (s:string):number;(n:number):string; }=3;
//~^ ERROR: Type 'number' is not assignable to type '(s: string) => number'.
var x4:{ x;y;z:number;f(n:number):string;f(s:string):number; }=3;
//~^ ERROR: Type 'number' is not assignable to type '{ x: any; y: any; z: number; f: (n: number) => string; }'.
var x5:{ (s:string):number;(n:number):string;x;y;z:number;f(n:number):string;f(s:string):number; }=3;
//~^ ERROR: Type 'number' is not assignable to type '(s: string) => number'.
var x6:{ z:number;f:{(n:number):string;(s:string):number;}; }=3;
//~^ ERROR: Type 'number' is not assignable to type '{ z: number; f: (n: number) => string; }'.
var x7:(s:string)=>boolean=3;
//~^ ERROR: Type 'number' is not assignable to type '(s: string) => boolean'.
var x8:{ z:I;[s:string]:{ x; y; };[n:number]:{x; y;};():boolean; }=3;
//~^ ERROR: Type 'number' is not assignable to type '() => boolean'.
//~| ERROR: Property 'z' of type 'I' is not assignable to 'string' index type '{ x: any; y: any; }'.
var x9:I=3;
///~^ ERROR: Type 'number' is not assignable to type 'I'.
var x10:I[][][][]=3;
//~^ ERROR: Type 'number' is not assignable to type 'I[][][][]'.
var x11:{z:I;x:boolean;}[][]=3;
//~^ ERROR: Type 'number' is not assignable to type '{ z: I; x: boolean; }[][]'.
var x12:{z:I;x:boolean;y:(s:string)=>boolean;w:{ z:I;[s:string]:{ x; y; };[n:number]:{x; y;};():boolean; };}[][]=3;
//~^ ERROR: Type 'number' is not assignable to type '{ z: I; x: boolean; y: (s: string) => boolean; w: () => boolean; }[][]'.
var x13:{ new(): number; new(n:number):number; x: string; w: {y: number;}; (): {}; } = 3;
//~^ ERROR: Type 'number' is not assignable to type '() => { }'.
var x14:{ f(x:number):boolean; p; q; ():string; }=3;
//~^ ERROR: Type 'number' is not assignable to type '() => string'. 
var x15:number=C;
//~^ ERROR: Type 'typeof C' is not assignable to type 'number'.


