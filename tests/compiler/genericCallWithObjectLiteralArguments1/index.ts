// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallWithObjectLiteralArguments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T>(n: { x: T; y: T }, m: T) { return m; }
// these are all errors
var x = foo({ x: 3, y: "" }, 4);
//~^ ERROR:  Type 'string' is not assignable to type 'number'.
var x2 = foo<number>({ x: 3, y: "" }, 4); 
//~^ ERROR:  Type 'string' is not assignable to type 'number'.
var x3 = foo<string>({ x: 3, y: "" }, 4); 
//~^ ERROR:  Type 'number' is not assignable to type 'string'.
var x4 = foo<number>({ x: "", y: 4 }, "");
//~^ ERROR:  Type 'string' is not assignable to type 'number'.
var x5 = foo<string>({ x: "", y: 4 }, "");
//~^ ERROR:  Type 'number' is not assignable to type 'string'.
