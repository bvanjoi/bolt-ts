// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyAccess2.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class C {
    public static foo() { }
    public get x() {
        return 1;
    }

    public static bar() { }
}

class D extends C {
    public static foo() {
        super.bar(); // OK
        super.x;  // error
        //~^ ERROR: Property 'x' does not exist on type 'typeof C'.
    }

    constructor() {
        super();
        super.bar(); // error
        //~^ ERROR: Property 'bar' does not exist on type 'C<D>'.
        super.x;  // error
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
    }

    public static get y() {
        super.bar(); // OK
        super.x; // error
        //~^ ERROR: Property 'x' does not exist on type 'typeof C'.
        return 1;
    }
}
