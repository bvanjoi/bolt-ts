// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallInsideClassDeclaration.ts`, Apache-2.0 License

class A {
}

class C {
}

class B extends A {
    constructor() {
      //~^ ERROR: Constructors for derived classes must contain a 'super' call.

        class D extends C {
            constructor() {
                super();
            }
        }
    }
}


const a = null;
class A0 extends a {
    constructor() {}
    //~^ ERROR: Constructors for derived classes must contain a 'super' call.
}

class A1 extends null {
    constructor() {}
}
