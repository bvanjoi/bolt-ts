// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/excessPropertyCheckIntersectionWithIndexSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

let x: { [x: string]: { a: 0 } } & { [x: string]: { b: 0 } };

x = { y: { a: 0 } };  // Error
//~^ ERROR: Property 'b' is missing.
x = { y: { a: 0, b: 0 } };
x = { y: { a: 0, b: 0, c: 0 } };  // Error
//~^ ERROR: Object literal may only specify known properties, and 'c' does not exist in type '{ a: 0; } & { b: 0; }'.

type A = { a: string };
type B = { b: string };

const yy: Record<string, A> & Record<string, B> = {
    foo: { a: '', b: '' },
};