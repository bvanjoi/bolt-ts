// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/staticsInConstructorBodies.ts`, Apache-2.0 License

class C {
	constructor() {
		static p1 = 0; // ERROR
    //~^ ERROR: Declaration or statement expected.
    //~| ERROR: '}' expected.
		static m1() {} // ERROR
	}
}	//~ ERROR: Declaration or statement expected.
