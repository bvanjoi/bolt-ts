// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/hidingCallSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface C {
    new (a: string): string;
}

interface D extends C {
    (a: string): number; // Should be ok
}

interface E {
    (a: string): {};
}

interface F extends E {
    (a: string): string;
}

var d: D;
d(""); // number
//~^ ERROR: Variable 'd' is used before being assigned.
new d(""); // should be string
//~^ ERROR: Variable 'd' is used before being assigned.

var f: F;
f(""); // string
//~^ ERROR: Variable 'f' is used before being assigned.

var e: E;
e(""); // {}
//~^ ERROR: Variable 'e' is used before being assigned.