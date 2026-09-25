// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorsInAmbientContext.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

declare namespace M {
    class C {
        get X() { return 1; }
        //~^ ERROR: An implementation cannot be declared in type contexts.
        set X(v) { }
        //~^ ERROR: An implementation cannot be declared in type contexts.
        //~| ERROR: Property 'X' implicitly has type 'any', because its set accessor lacks a parameter type annotation.

        static get Y() { return 1; }
        //~^ ERROR: An implementation cannot be declared in type contexts.
        static set Y(v) { }
        //~^ ERROR: An implementation cannot be declared in type contexts.
        //~| ERROR: Property 'Y' implicitly has type 'any', because its set accessor lacks a parameter type annotation.
    }
}

declare class C {
    get X() { return 1; }
    //~^ ERROR: An implementation cannot be declared in type contexts.
    set X(v) { }
    //~^ ERROR: An implementation cannot be declared in type contexts.
    //~| ERROR: Property 'X' implicitly has type 'any', because its set accessor lacks a parameter type annotation.

    static get Y() { return 1; }
    //~^ ERROR: An implementation cannot be declared in type contexts.
    static set Y(v) { }
    //~^ ERROR: An implementation cannot be declared in type contexts.
    //~| ERROR: Property 'Y' implicitly has type 'any', because its set accessor lacks a parameter type annotation.
}