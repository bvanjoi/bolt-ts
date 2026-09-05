// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/requiredMappedTypeModifierTrumpsVariance.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const a: Required<{ a?: 1; x: 1 }> = { a: 1, x: 1 };
const b: Required<{ b?: 1; x: 1 }> = { b: 1, x: 1 };
export let A = a;
export let B = b;
A = b; // Should Error
//~^ ERROR: Property 'a' is missing.
B = a; // Should Error
//~^ ERROR: Property 'b' is missing.

a.b; // Property 'b' does not exist on type 'Required<{ a?: 1; x: 1; }>'.
//~^ ERROR: Property 'b' does not exist on type 'Required<{ a: undefined | 1; x: 1; }>'.
b.a; // Property 'a' does not exist on type 'Required<{ b?: 1; x: 1; }>'.
//~^ ERROR: Property 'a' does not exist on type 'Required<{ b: undefined | 1; x: 1; }>'.

interface Foo<T> {
    a: Required<T>;
}
const aa: Foo<{ a?: 1; x: 1 }> = { a: { a: 1, x: 1 } };
const bb: Foo<{ b?: 1; x: 1 }> = { a: { b: 1, x: 1 } };
export let AA = aa;
export let BB = bb;
AA = bb; // Should Error
//~^ ERROR: Property 'a' is missing.
BB = aa; // Should Error
//~^ ERROR: Property 'b' is missing.

aa.a.b; // Property 'b' does not exist on type 'Required<{ a?: 1; x: 1; }>'.
//~^ ERROR: Property 'b' does not exist on type 'Required<{ a: undefined | 1; x: 1; }>'.
bb.a.a; // Property 'a' does not exist on type 'Required<{ b?: 1; x: 1; }>'.
//~^ ERROR: Property 'a' does not exist on type 'Required<{ b: undefined | 1; x: 1; }>'.
