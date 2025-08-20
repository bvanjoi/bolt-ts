// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/functionCall7.ts`, Apache-2.0 License

module m1 { export class c1 { public a; }}
function foo(a:m1.c1){ a.a = 1; }; 
var myC = new m1.c1(); 
foo(myC); 
foo(myC, myC); 
//~^ ERROR: Expected 1 arguments, but got 2.
foo(4);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'c1'.
foo();
//~^ ERROR: Expected 1 arguments, but got 0.
