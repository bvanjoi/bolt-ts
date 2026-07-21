// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/flowControlTypeGuardThenSwitch.ts`, Apache-2.0 License

//@compiler-options: target=es2015
enum Kind {
    A,
    B,
}

interface Base {
    kind: Kind;
}

interface A extends Base {
    kind: Kind.A;
    yar: any;
}

interface B extends Base {
    kind: Kind.B;
    gar: any;
}

type Both = A | B;
function isBoth(x: Base): x is Both {
    return true;
}

let foo: Base = undefined;
//~^ ERROR: Type 'undefined' is not assignable to type 'Base'.
if (isBoth(foo)) {
    switch (foo.kind) {
        case Kind.A:
            const myA: A = foo; // Should not be an error
            break;
        case Kind.B:
            const myB: B = foo;
            break;
    }
}