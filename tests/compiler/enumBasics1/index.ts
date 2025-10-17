// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumBasics1.ts`, Apache-2.0 License

enum E {
	A = 1,
	B,
	C
}

/*
var a: E;
var b = E["B"]; // shouldn't error


function foo(e: E) {}

foo(a); // shouldn't error


class C {
	public e: E;

	public m(): E { return this.e; } // shouldn't error
}


var e = E; // shouldn't error
*/
E.A.A; // should error
//~^ ERROR: Property 'A' does not exist on type 'E.A'.


enum E2 {
	A,
	B,
}

enum E2 { // should error for continued autonumbering
	C, //~ ERROR: In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element.
	D,
}