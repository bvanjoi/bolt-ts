// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/stringIndexerAssignments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x: { [index: string]: string; one: string; };
declare var a: { one: string; };
declare var b: { one: number; two: string; };
x = a;
x = b; // error
//~^ ERROR: Type '{ one: number; two: string; }' is not assignable to type '{ [index: string]: string }
