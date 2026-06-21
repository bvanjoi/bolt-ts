// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleNewExportBug.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace mod1 {
	interface mInt {
		new (bar:any):any;
        foo (bar:any):any;
	}
 
    class C { public moo() {}}
}

var c : mod1.C; // ERROR: C should not be visible
//~^ ERROR: Namespace 'mod1' has no exported member 'C'.


