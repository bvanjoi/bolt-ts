// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatBug5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo1(x: { a: number; }) { }
foo1({ b: 5 });
//~^ ERROR: Object literal may only specify known properties, and 'b' does not exist in type '{ a: number; }'.

function foo2(x: number[]) { }
foo2(["s", "t"]);
//~^ ERROR: Type 'string' is not assignable to type 'number'.
//~| ERROR: Type 'string' is not assignable to type 'number'.

function foo3(x: (n: number) =>number) { };
foo3((s:string) => { });
//~^ ERROR: Argument of type '(s: string) => void' is not assignable to parameter of type '(n: number) => number'.
foo3((n) => { return; });
//~^ ERROR: Argument of type '(n: number) => undefined' is not assignable to parameter of type '(n: number) => number'.
