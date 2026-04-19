// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/numericIndexerConstraint5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x = { name: "x", 0: new Date() };
var z: { [name: number]: string } = x;
//~^ ERROR: Type '{ name: string; 0: Date; }' is not assignable to type '{ [name: number]: string }'.