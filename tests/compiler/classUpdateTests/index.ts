// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classUpdateTests.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

//
// test codegen for instance properties
//
class A {
	public p1 = 0;
	private p2 = 0;
	p3;
}

class B {
	public p1 = 0;
	private p2 = 0;
	p3;

	constructor() {}
}

class C {
	constructor(public p1=0, private p2=0, p3=0) {}
}

//
// test requirements for super calls
//
class D { // NO ERROR
	
}

class E extends D { // NO ERROR
	public p1 = 0;
}

class F extends E {
	constructor() {} // ERROR - super call required
  //~^ ERROR: Constructors for derived classes must contain a 'super' call.
}

class G extends D {
	public p1 = 0;
	constructor() { super(); } // NO ERROR
}

class H {
	constructor() { super(); } // ERROR - no super call allowed
  //~^ ERROR: 'super' can only be referenced in a derived class.
}

class I extends Object {
	constructor() { super(); } // ERROR - no super call allowed
}

class J extends G {
	constructor(public p1:number) {
		super(); // NO ERROR
	}
}

class K extends G {
	constructor(public p1:number) { // ERROR
		var i = 0;
		super();
	}
}

class L extends G {
  //~^ ERROR: Class 'L' incorrectly extends base class 'G'.
	constructor(private p1:number) {
		super(); // NO ERROR
	}
}

class M extends G {
  //~^ ERROR: Class 'M' incorrectly extends base class 'G'.
	constructor(private p1:number) { // ERROR
		var i = 0;
		super();
	}
}

//
// test this reference in field initializers
//
class N {
	public p1 = 0;
	public p2 = this.p1;

	constructor() {
		this.p2 = 0;
	}
}

//
// test error on property declarations within class constructors
//
class O {
	constructor() {
		public p1 = 0; // ERROR
    //~^ ERROR: Declaration or statement expected.
    //~| ERROR: '}' expected.
	}
}
//~^ ERROR: Declaration or statement expected.

class P {
	constructor() {
		private p1 = 0; // ERROR
    //~^ ERROR: Declaration or statement expected.
    //~| ERROR: '}' expected.
	}
}
//~^ ERROR: Declaration or statement expected.

class Q {
	constructor() {
		public this.p1 = 0; // ERROR
    //~^ ERROR: Declaration or statement expected.
    //~| ERROR: '}' expected.
    //~| ERROR: Unexpected keyword or identifier.
	}
}
//~^ ERROR: Declaration or statement expected.

class R {
	constructor() {
		private this.p1 = 0; // ERROR
    //~^ ERROR: Declaration or statement expected.
    //~| ERROR: '}' expected.
    //~| ERROR: Unexpected keyword or identifier.
	}
}
//~^ ERROR: Declaration or statement expected.
