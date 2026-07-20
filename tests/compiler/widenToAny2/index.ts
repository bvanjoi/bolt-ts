// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/widenToAny2.ts`, Apache-2.0 License

function foo3<T>(x: T[]): T {
  return undefined;
  //~^ ERROR: Type 'undefined' is not assignable to type 'T'.
}
var z3:number = foo3([undefined, "def"]);  // Type is any, but should be string
//~^ ERROR: Type 'undefined | string' is not assignable to type 'number'.
