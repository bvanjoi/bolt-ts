// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/hidingConstructSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es6

interface C {
    (a: string): string;
}

interface D extends C {
    new (a: string): number; // Should be ok
}

interface E {
    new (a: string): {};
}

interface F extends E {
    new (a: string): string;
}

var d: D;
d(""); // string
//~^ ERROR: Variable 'd' is used before being assigned.
new d(""); // should be number
//~^ ERROR: Variable 'd' is used before being assigned.

var f: F;
new f(""); // string
//~^ ERROR: Variable 'f' is used before being assigned.

var e: E;
new e(""); // {}
//~^ ERROR: Variable 'e' is used before being assigned.
