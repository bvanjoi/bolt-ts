// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/returnTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f<T>(a: T): T { } // error, no return statement
//~^ ERROR:  A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
function f2<T>(a: T): T { return T; } // bug was that this satisfied the return statement requirement
//~^ ERROR: Cannot find name 'T'.