// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericArrayAssignment1.ts`, Apache-2.0 License

var s: string[];
var n: number[];

s = n;
//~^ ERROR: Type 'number[]' is not assignable to type 'string[]'.