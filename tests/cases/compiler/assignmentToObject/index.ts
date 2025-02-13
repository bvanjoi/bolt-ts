// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/assignmentToObject.ts`, Apache-2.0 License

var a = { toString: 5 };
var b: {} = a;  // ok
var c: Object = a;  // should be error
//~^ ERROR: Type '{ toString: number; }' is not assignable to type 'Object'.