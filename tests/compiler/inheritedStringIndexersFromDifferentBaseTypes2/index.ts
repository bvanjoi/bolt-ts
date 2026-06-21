// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritedStringIndexersFromDifferentBaseTypes2.ts`, Apache-2.0 License

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
interface C extends A, B { } // ok

interface D {
    [s: number]: {};
}
interface E extends A, D { } // error
//~^ ERROR: 'number' index type '{ }' is not assignable to 'string' index type '{ a: any; }'.

interface F extends A, D {
    [s: number]: {
        a;
    };
} // ok because we overrode D's number index signature