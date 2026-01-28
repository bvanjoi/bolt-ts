// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericIsNeverEmptyObject.ts`, Apache-2.0 License

//@compiler-options: strict

function test<T extends { a: string }>(obj: T) {
    let { a, ...rest } = obj;
    return { ...rest, b: a };
}

let o1 = { a: 'hello', x: 42 };
let o2: { b: string, x: number } = test(o1);
let o3: { b: string, x: string } = test(o1);
//~^ ERROR: Type 'Pick & { b: string; }' is not assignable to type '{ b: string; x: string; }'.
