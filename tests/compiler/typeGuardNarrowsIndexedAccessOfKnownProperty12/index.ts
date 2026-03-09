// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty12.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=es2015


enum E {
    A = "A",
    B = "B"
}

declare const m: { [K in E]: string | null };

if (m[E.A] !== null) {
    m[E.A].toString(); // string
    const a: number = m[E.A];
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
}
