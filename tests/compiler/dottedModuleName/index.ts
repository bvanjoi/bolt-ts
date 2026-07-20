// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/dottedModuleName.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    export namespace N {
	export function f(x:number)=>2*x;
  //~^ ERROR: Expected '{'.
  //~| ERROR: Declaration or statement expected.
	export namespace X.Y.Z {
  //~^ ERROR: A namespace declaration is only allowed at the top level of a namespace or module.
	    export var v2=f(v);
      //~^ ERROR: Cannot find name 'v'.
	}
    }
}



namespace M.N {
    export namespace X {
	export namespace Y.Z {
	    export var v=f(10);
      //~^ ERROR: Cannot find name 'f'.
	}
    }
}
//~ ERROR: Expected '}'.