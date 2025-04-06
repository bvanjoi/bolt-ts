// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonNullableAndObjectIntersections.ts`, Apache-2.0 License

// These should all resolve to never

type T0 = NonNullable<null>;
type T1 = NonNullable<undefined>;
type T2 = null & {};
type T3 = undefined & {};
type T4 = null & undefined;
type T6 = null & { a: string } & {};

// Repro from #50519

type NonNullableNew<T> = T & {};
type NonNullableOld<T> = T extends null | undefined ? never : T;

type TestNew = NonNullableNew<null>;
type TestOld = NonNullableOld<null>;


let a0: T0 = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a1: T1 = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a2: T2 = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a3: T3 = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a4: T4 = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a6: T6 = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a7: TestNew = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a8: TestOld = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.
let a9: NonNullableOld<undefined> = false;
//~^ ERROR: Type 'false' is not assignable to type 'never'.

{
  type W<T> = (keyof {[P in keyof T as P ]: never}) & keyof T

  // `W<T>` should be assignable to `PropertyKey`
  type A<_T, _K extends PropertyKey> = unknown;
  type Test<T extends object> = A<T, W<T>>;
}