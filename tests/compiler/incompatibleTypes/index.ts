// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/incompatibleTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface IFoo1 {
    p1(): number;
}

class C1 implements IFoo1 { // incompatible on the return type
    public p1() {
  //~^ ERROR: Property 'p1' in type 'C1<C1>' is not assignable to the same property in base type 'IFoo1'.
        return "s";
    }
}

interface IFoo2 {
    p1(s:string): number;
}

class C2 implements IFoo2 { // incompatible on the param type
    public p1(n:number) {
  //~^ ERROR: Property 'p1' in type 'C2<C2>' is not assignable to the same property in base type 'IFoo2'.
        return 0;
    }
}

interface IFoo3 {
    p1: string;
}

class C3 implements IFoo3 { // incompatible on the property type
    public p1: number;
  //~^ ERROR: Property 'p1' in type 'C3<C3>' is not assignable to the same property in base type 'IFoo3'.
}

interface IFoo4 {
    p1: { a: { a: string; }; b: string; };
}

class C4 implements IFoo4 { // incompatible on the property type
    public p1: { c: { b: string; }; d: string; };
    //~^ ERROR: Property 'a' is missing.
    //~| ERROR: Property 'b' is missing.
}

function if1(i: IFoo1): void;
function if1(i: IFoo2): void;
function if1(a: any) { }
var c1: C1;
var c2: C2;
if1(c1);
//~^ ERROR: No overload matches this call.

function of1(n: { a: { a: string; }; b: string; }): number;
function of1(s: { c: { b: string; }; d: string; }): string;
function of1(a: any) { return null; }

of1({ e: 0, f: 0 });
//~^ ERROR: No overload matches this call.

interface IMap {
 [key:string]:string;
}

function foo(fn:() => void) {
 
}

function bar() {
 var map:IMap;
 foo(() => {
  map = {};
 });
}

var o1: { a: { a: string; }; b: string; } = { e: 0, f: 0 };
//~^ ERROR: Object literal may only specify known properties, and 'e' does not exist in type '{ a: { a: string; }; b: string; }'.

var a1 = [{ e: 0, f: 0 }, { e: 0, f: 0 }, { e: 0, g: 0 }];



var i1c1: { (): string; } = 5;
//~^ ERROR: Type 'number' is not assignable to type '() => string'.

var fp1: () =>any = a => 0;
//~^ ERROR: Type '(a: any) => number' is not assignable to type '() => any'.