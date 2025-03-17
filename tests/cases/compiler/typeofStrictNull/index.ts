// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeofStrictNull.ts`, Apache-2.0 License

let a: number;
let b: typeof a;

let c: string = b;
//~^ ERROR: Type 'number' is not assignable to type 'string'.