// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticPropSuper.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class A {
}

class B extends A {
    public static s: number = 9;

    constructor() {
        var x = 1; // should not error
        super();
    }
}

class C extends A {
    public p: number = 10;

    constructor() {
      //~^ ERROR: Constructors for derived classes must contain a 'super' call.
        var x = 1; // should error
    }
}

class D extends A {
    private p: number = 11;

    constructor() {
      //~^ ERROR: Constructors for derived classes must contain a 'super' call.
        var x = 1; // should error
    }
}

class E extends A {
    p: number = 12;

    constructor() {
      //~^ ERROR: Constructors for derived classes must contain a 'super' call.
        var x = 1; // should error
    }
}