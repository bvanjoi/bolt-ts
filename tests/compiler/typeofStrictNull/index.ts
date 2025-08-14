// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeofStrictNull.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks

let a: number;
let b: typeof a;

let d: typeof a = 42;
let c: string = d;
//~^ ERROR: Type 'number' is not assignable to type 'string'.