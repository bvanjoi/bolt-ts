// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/captureSuperPropertyAccessInSuperCall01.ts`, Apache-2.0 License

class A {
	constructor(f: () => string) {
	}
	public blah(): string { return ""; }
}

class B extends A {
	constructor() {
		super(() => { return super.blah(); })
	}
}