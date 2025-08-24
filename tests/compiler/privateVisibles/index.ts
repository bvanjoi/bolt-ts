// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/privateVisibles.ts`, Apache-2.0 License

class Foo {
	private pvar = 0;
	constructor() {
	    var n = this.pvar;
	}

	public meth() { var q = this.pvar;}
}
