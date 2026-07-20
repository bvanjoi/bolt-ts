// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexerConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A { a: number; }
interface B extends A { b: number; }

// Good case
interface D {
    [s: string]: A;
}
interface D {
    [n: number]: B;
}

// Bad case
interface E {
    [s: string]: B;
}
interface E {
    [n: number]: A;
    //~^ ERROR: 'number' index type 'A' is not assignable to 'string' index type 'B'.
}

// Inheritance
interface F {
    [s: string]: B;
}
interface G extends F {
    [n: number]: A;
    //~^ ERROR: 'number' index type 'A' is not assignable to 'string' index type 'B'.
}

// Other way
interface H {
    [n: number]: A;
}
interface I extends H {
    [s: string]: B;
    //~^ ERROR: 'number' index type 'A' is not assignable to 'string' index type 'B'.
}

// With hidden indexer
interface J {
    [n: number]: {};
}
interface K extends J {
    [n: number]: A;
    //~^ ERROR: 'number' index type 'A' is not assignable to 'string' index type 'B'.
    [s: string]: B;
}