// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritedStringIndexersFromDifferentBaseTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    [s: string]: number;
}
interface B {
    [s: string]: number;
}
interface C extends A, B { } // ok

interface D {
    [s: string]: string;
}
interface E extends A, D { } // error
//~^ ERROR: 'number' index signatures are incompatible.


// Same tests for number indexer
interface A2 {
    [s: number]: number;
}
interface B2 {
    [s: number]: number;
}
interface C2 extends A2, B2 { } // ok

interface D2 {
    [s: number]: string;
}
interface E2 extends A2, D2 { } // error
//~^ ERROR: 'number' index signatures are incompatible.