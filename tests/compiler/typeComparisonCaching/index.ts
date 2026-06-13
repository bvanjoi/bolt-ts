// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeComparisonCaching.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    p: C;
    s: string;
}

interface B {
    p: D;
    s: number;
}

interface C {
    q: A;
}

interface D {
    q: B;
}

var a: A;
declare var b: B;
var c: C;
declare var d: D;

a = b;
//~^ ERROR: Type 'B' is not assignable to type 'A'.
c = d; // Should not be allowed
//~^ ERROR: Type 'D' is not assignable to type 'C'.