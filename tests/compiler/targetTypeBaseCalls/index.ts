// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/targetTypeBaseCalls.ts`, Apache-2.0 License

function foo(x: (s: string) => void) { }
 
 
 
class Foo { constructor(x: (s: string) => void){} }
 
 
 
foo(function(s) { s = 5 });  // Error, can’t assign number to string
//~^ ERROR: Type 'number' is not assignable to type 'string'. 
 
 
new Foo(function(s) { s = 5 });  // error, if types are applied correctly
//~^ ERROR: Type 'number' is not assignable to type 'string'. 
 
 
 
class Bar extends Foo { constructor() { super(function(s) { s = 5 }) } }  // error, if types are applied correctly
//~^ ERROR: Type 'number' is not assignable to type 'string'. 
