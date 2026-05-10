// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionArgShadowing.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A { foo() { } }
class B { bar() { } }
function foo(x: A) {
   var x: B = new B();
   //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'A', but here has type 'B'.
     x.bar(); // the property bar does not exist on a value of type A
		//~^ ERROR: Property 'bar' does not exist on type 'A'.
}
 
class C {
	constructor(public p: number) {
		var p: string;
		//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'p' must be of type 'number', but here has type 'string'.

		var n: number = p;
	}
}