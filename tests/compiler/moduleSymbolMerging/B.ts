///<reference path="A.ts" preserve="true" />
namespace A { ; }
namespace B {
	export function f(): A.I { return null; }
  //~^ ERROR: Type 'null' is not assignable to type 'A.I'.
}

