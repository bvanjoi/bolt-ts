// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/chainedAssignment2.ts`, Apache-2.0 License

var a: string;
var b: number;
var c: boolean;
var d: Date;
var e: RegExp;

a = b = c = d = e = null;
//~^ ERROR: Type 'null' is not assignable to type 'RegExp'.
//~| ERROR: Type 'null' is not assignable to type 'Date'.
//~| ERROR: Type 'null' is not assignable to type 'boolean'.
//~| ERROR: Type 'null' is not assignable to type 'number'.
//~| ERROR: Type 'null' is not assignable to type 'string'.

