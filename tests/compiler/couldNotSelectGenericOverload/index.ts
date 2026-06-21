// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/couldNotSelectGenericOverload.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function makeArray<T>(items: T[]): T[] { return items; }
var b = [1, ""];
var b1G = makeArray(1, ""); // any, no error
//~^ ERROR: Expected 1 arguments, but got 2.
var b2G = makeArray(b); // any[]

function makeArray2(items: any[]): any[] { return items; }
var b3G = makeArray2(1, ""); // error
//~^ ERROR: Expected 1 arguments, but got 2.
