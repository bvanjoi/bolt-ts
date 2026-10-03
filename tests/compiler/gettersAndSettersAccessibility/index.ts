// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/gettersAndSettersAccessibility.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C99 {
	private get Baz():number { return 0; } //~ERROR: A get accessor must be at least as accessible as the setter.
	public set Baz(n:number) {} // error - accessors do not agree in visibility
}
