// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceMemberValidation.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface i1 { name: string; }
interface i2 extends i1 { name: number; yo: string; }
//~^ ERROR: Interface 'i2' incorrectly extends interface 'i1'.

interface foo {
 bar():any;
 //~^ ERROR: Property 'bar' of type '() => any' is not assignable to 'string' index type 'number'.
 //~| ERROR: Property 'bar' of type '() => any' is not assignable to 'string' index type 'number'.
 bar():any;
 new():void;
 new():void;
 [s:string]:number;
 //~^ ERROR: Duplicate index signature for type 'string'.
 [s:string]:number;
 //~^ ERROR: Duplicate index signature for type 'string'.
}

type A = string;
interface bar {
 [s:A]:number;
 //~^ ERROR: Duplicate index signature for type 'string'.
 [s:A]:number;
 //~^ ERROR: Duplicate index signature for type 'string'.
}

interface baz {
 [s:A]:number;
}