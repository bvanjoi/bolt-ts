// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/hidingIndexSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    [a: string]: {};
}

interface B extends A {
    [a: string]: number; // Number is not a subtype of string.  Should error.
}

var b: B;
b[""]; // Should be number
//~^ ERROR: Variable 'b' is used before being assigned.
var a: A;
a[""]; // Should be {}
//~^ ERROR: Variable 'a' is used before being assigned.
