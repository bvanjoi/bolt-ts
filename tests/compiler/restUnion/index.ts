// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var union: { a: number, c: boolean } | { a: string, b: string };

var rest1: { c: boolean } | { b: string };
var {a, ...rest1 } = union;
//~^ ERROR: Variable 'union' is used before being assigned.
//~| ERROR: Variable 'union' is used before being assigned.
//~| ERROR: Variable 'union' is used before being assigned.

var undefinedUnion: { n: number } | undefined;
var rest2: {};
var {n, ...rest2 } = undefinedUnion;
//~^ ERROR: Property '"n"' does not exist on type 'undefined | { n: number; }'.


var nullUnion: { n: number } | null;
var rest3: {};
var {n, ...rest3 } = nullUnion;
//~^ ERROR: Property '"n"' does not exist on type 'null | { n: number; }'.
//~| ERROR: Variable 'nullUnion' is used before being assigned.
//~| ERROR: Variable 'nullUnion' is used before being assigned.
//~| ERROR: Variable 'nullUnion' is used before being assigned.
