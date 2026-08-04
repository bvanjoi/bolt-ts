// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatWithOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f1(x: string): number { return null; }
//~^ ERROR: Type 'null' is not assignable to type 'number'.

function f2(x: string): string { return null; }
//~^ ERROR: Type 'null' is not assignable to type 'string'.

function f3(x: number): number { return null; }
//~^ ERROR: Type 'null' is not assignable to type 'number'.

function f4(x: string): string;

function f4(x: number): number;

function f4(x: any): any { return undefined; }

var g: (s1: string) => number;

g = f1; // OK 

g = f2; // Error
//~^ ERROR: Type '(x: string) => string' is not assignable to type '(s1: string) => number'.

g = f3; // Error
//~^ ERROR: Type '(x: number) => number' is not assignable to type '(s1: string) => number'.

g = f4; // Error
//~^ ERROR: Type '(x: string) => string' is not assignable to type '(s1: string) => number'.


f4('42');
f4(42);

class C {
    constructor(x: string);
constructor(x: any) {}
}

var d: new(x: number) => void;

d = C; // Error
//~^ ERROR: Type 'typeof C' is not assignable to type 'new (x: number) => void'.