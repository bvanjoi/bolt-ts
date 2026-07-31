// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexerAssignability.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var a: { [s: string]: string; };
var b: { [n: number]: string; };
var c: {};

a = b;
//~^ ERROR: Variable 'b' is used before being assigned.
a = c;
//~^ ERROR: Variable 'c' is used before being assigned.
b = a;
b = c;
//~^ ERROR: Variable 'c' is used before being assigned.
c = a;
c = b;