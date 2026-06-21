// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyAccessInSuperCall01.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
	constructor(f: string) {
	}
	public blah(): string { return ""; }
}

class B extends A {
	constructor() {
		super(super.blah())
    //~^ ERROR: 'super' must be called before accessing a property of 'super' in the constructor of a derived class.
	}
}
