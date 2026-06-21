// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritedMembersAndIndexSignaturesFromDifferentBases.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A {
    [s: string]: {
        a;
    };
}
interface B {
    [s: number]: {
        a;
        b;
    };
}
interface C {
    m: {};
}

interface D extends A, B, C { } // error because m is not a subtype of {a;}
//~^ ERROR: Property 'm' of type '{ }' is not assignable to 'string' index type '{ a: any; }'.

interface E {
    0: {};
}

interface F extends A, B, E { } // error because 0 is not a subtype of {a; b;}
//~^ ERROR: Property '0' of type '{ }' is not assignable to 'number' index type '{ a: any; b: any; }'.
//~| ERROR: Property '0' of type '{ }' is not assignable to 'string' index type '{ a: any; }'.

interface G extends A, B, C, E { } // should only report one error
//~^ ERROR: Property '0' of type '{ }' is not assignable to 'number' index type '{ a: any; b: any; }'.
//~| ERROR: Property '0' of type '{ }' is not assignable to 'string' index type '{ a: any; }'.
//~| ERROR: Property 'm' of type '{ }' is not assignable to 'string' index type '{ a: any; }'.

interface H extends A, F { } // Should report no error at all because error is internal to F