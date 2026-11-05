// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superAccess2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class P {
    x() { }
    static y() { }
}

class Q extends P {
    xx = super;
    //~^ ERROR: 'super' must be followed by an argument list or member access.
    //~| ERROR: Identifier expected.
    static yy = super; // error for static initializer accessing super
    //~^ ERROR: 'super' must be followed by an argument list or member access.
    //~| ERROR: Identifier expected.

    // Super is not allowed in constructor args
    constructor(public z = super, zz = super, zzz = () => super) {
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
        //~| ERROR: Identifier expected.
        //~| ERROR: Identifier expected.
        //~| ERROR: 'super' must be called before accessing a property of 'super' in the constructor of a derived class.
        //~| ERROR: 'super' must be called before accessing a property of 'super' in the constructor of a derived class.
        //~| ERROR: 'super' cannot be referenced in constructor arguments.
        //~| ERROR: 'super' cannot be referenced in constructor arguments.
        super();
    }

    foo(zz = super) {
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
        super.x();
        super.y(); // error
        //~^ ERROR: Property 'y' does not exist on type 'P<Q>'.
    }

    static bar(zz = super) {
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
        super.x(); // error
        //~^ ERROR: Property 'x' does not exist on type 'typeof P'.
        super.y();
    }
}
