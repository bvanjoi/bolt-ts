// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleExports1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export namespace TypeScript.Strasse.Street {
	export class Rue {
		public address:string;
    //~^ ERROR: Property 'address' has no initializer and is not definitely assigned in the constructor.
	}	
}

var rue = new TypeScript.Strasse.Street.Rue();

rue.address = "1 Main Street";

void 0;

if (!module.exports) module.exports = "";
//~^ ERROR: Cannot find name 'module'.
//~| ERROR: Cannot find name 'module'.
