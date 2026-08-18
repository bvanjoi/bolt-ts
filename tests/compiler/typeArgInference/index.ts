// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
    f<T, U>(a1: { a: T; b: U }[], a2: { a: T; b: U }[]): { c: T; d: U };
    g<T, U>(...arg: { a: T; b: U }[][]): { c: T; d: U };
}
var o = { a: 3, b: "test" };
var x: I;
var t1 = x.f([o], [o]);
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
var t1: { c: number; d: string }; 
var t2 = x.f<number, string>([o], [o]);
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
var t2: { c: number; d: string }; 
var t3 = x.g([o], [o]);
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
var t3: { c: number; d: string };
var t4 = x.g<number, string>([o], [o]);
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
var t4: { c: number; d: string };
