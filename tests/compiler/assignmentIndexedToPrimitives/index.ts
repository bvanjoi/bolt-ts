// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentIndexedToPrimitives.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const n1: number = [0];
//~^ ERROR: Type 'number[]' is not assignable to type 'number'.
const n2: number = ["0"];
//~^ ERROR: Type 'string[]' is not assignable to type 'number'.
const n3: number = [0, "1"];
//~^ ERROR: Type '(number | string)[]' is not assignable to type 'number'.
const n4: 0 = [0];
//~^ ERROR: Type 'number[]' is not assignable to type '0'.

const s1: string = [0];
//~^ ERROR: Type 'number[]' is not assignable to type 'string'.
const s2: string = ["0"];
//~^ ERROR: Type 'string[]' is not assignable to type 'string'.
const s3: string = [0, "1"];
//~^ ERROR: Type '(number | string)[]' is not assignable to type 'string'.
const s4: "01" = ["0", "1"];
//~^ ERROR: Type 'string[]' is not assignable to type '"01"'.

const no1: number = { 0: 1 };
//~^ ERROR: Type '{ 0: number; }' is not assignable to type 'number'.
const so1: string = { 0: 1 };
//~^ ERROR: Type '{ 0: number; }' is not assignable to type 'string'.
const so2: string = { "0": 1 };
//~^ ERROR: Type '{ 0: number; }' is not assignable to type 'string'.
const so3: string = { 0: "1" };
//~^ ERROR: Type '{ 0: string; }' is not assignable to type 'string'.
