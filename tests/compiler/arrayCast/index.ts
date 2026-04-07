// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayCast.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Should fail. Even though the array is contextually typed with { id: number }[], it still
// has type { foo: string }[], which is not assignable to { id: number }[].
<{ id: number; }[]>[{ foo: "s" }];
//~^ ERROR: Object literal may only specify known properties, and 'foo' does not exist in type '{ id: number; }'.

// Should succeed, as the {} element causes the type of the array to be {}[]
<{ id: number; }[]>[{ foo: "s" }, {}]; 
