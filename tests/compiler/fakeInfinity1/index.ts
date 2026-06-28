// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fakeInfinity1.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: module=commonjs
//@compiler-options: strict
//@compiler-options: declaration

// These are not actually the real infinity.
export type PositiveInfinity = 1e999;
export type NegativeInfinity = -1e999;

export type TypeOfInfinity = typeof Infinity;
export type TypeOfNaN = typeof NaN;

type A = 1e999;
type B = 1e9999;

declare let a: A;
declare let b: B;

a = b;
b = a;

a = Infinity;
//~^ ERROR: Type 'number' is not assignable to type 'Infinity'.
a = 1e999;
a = 1e9999;

export type Oops = 123456789123456789123456789123456789123456789123456789;
export const oops = 123456789123456789123456789123456789123456789123456789;