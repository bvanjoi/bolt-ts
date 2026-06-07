// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleVisibilityTest2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace OuterMod {
	export function someExportedOuterFunc() { return -1; }

	export namespace OuterInnerMod {
		export function someExportedOuterInnerFunc() { return "foo"; }
	}
}

import OuterInnerAlias = OuterMod.OuterInnerMod;

namespace M {

	namespace InnerMod {
		export function someExportedInnerFunc() { return -2; }
	}

	enum E {
		A,
		B,
		C,
	}

	var x = 5;
	export declare var exported_var;

	var y = x + x;


	interface I {
		someMethod():number;
	}

	 class B {public b = 0;}

	 export class C implements I {
		public someMethodThatCallsAnOuterMethod() {return OuterInnerAlias.someExportedOuterInnerFunc();}
		public someMethodThatCallsAnInnerMethod() {return InnerMod.someExportedInnerFunc();}
		public someMethodThatCallsAnOuterInnerMethod() {return OuterMod.someExportedOuterFunc();}
		public someMethod() { return 0; }
		public someProp = 1;

		constructor() {
		    function someInnerFunc() { return 2; }
            var someInnerVar = 3;
		}
		
	}

	var someModuleVar = 4;

	function someModuleFunction() { return 5;}
}

namespace M {
	export var c = x;
  //~^ ERROR: Cannot find name 'x'.
	export var meb = M.E.B;
  //~^ ERROR: Property 'E' does not exist on type 'typeof M'.
}

var cprime : M.I = <M.I>null;
//~^ ERROR: Namespace 'M' has no exported member 'I'.
//~| ERROR: Namespace 'M' has no exported member 'I'.

var c = new M.C();
var z = M.x;
//~^ ERROR: Property 'x' does not exist on type 'typeof M'.
var alpha = M.E.A;
//~^ ERROR: Property 'E' does not exist on type 'typeof M'.
var omega = M.exported_var;
c.someMethodThatCallsAnOuterMethod();
