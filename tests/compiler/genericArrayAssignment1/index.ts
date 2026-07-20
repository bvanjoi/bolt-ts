// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericArrayAssignment1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var s: string[];
declare var n: number[];

s = n;
//~^ ERROR: Type 'number[]' is not assignable to type 'string[]'.
