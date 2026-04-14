// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorMessageOnIntersectionsWithDiscriminants01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noEmit


export type Common = { test: true } | { test: false };
export type A = Common & { foo: 1 };
export type B = Common & { bar: 1 };

declare const a: A;
declare let b: B;

b = a;
//~^ ERROR: Type 'A' is not assignable to type 'B'.
