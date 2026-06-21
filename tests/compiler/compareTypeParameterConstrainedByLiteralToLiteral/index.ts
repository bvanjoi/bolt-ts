// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/compareTypeParameterConstrainedByLiteralToLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T extends "a" | "b">(t: T) {
    t === "a";  // Should be allowed
    t === "x";  // Should be error
    //~^ ERROR: This comparison appears to be unintentional because the types 'T' and '"x"' have no overlap.
}