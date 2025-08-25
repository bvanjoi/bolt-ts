// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveConditionalCrash1.ts`, Apache-2.0 License

type C1<T> = [T extends string ? C1<T> : never][0];
type C2<T> = [T extends string ? [C2<T>] : never][0];

let c10: C1<string> = 42;
//~^ ERROR: Type instantiation is excessively deep and possibly infinite.
//~| ERROR: Type instantiation is excessively deep and possibly infinite.
let c11: C1<number> = n();
// let c20: C2<string> = []; // TODO:
let c21: C2<number> = n();

function n(): never {
    throw Error()
}