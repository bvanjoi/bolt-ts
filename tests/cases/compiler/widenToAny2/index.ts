// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/widenToAny2.ts`, Apache-2.0 License

function foo3<T>(x: T[]): T {
  return undefined;
}
var z3:number = foo3([undefined, "def"]);  // Type is any, but should be string
//~^ ERROR: Type 'string' is not assignable to type 'number'.
