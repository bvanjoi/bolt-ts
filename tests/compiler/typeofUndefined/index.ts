// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeofUndefined.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var x: typeof undefined;
var x: any; // shouldn't be an error since type is the same as the first declaration
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'undefined', but here has type 'any'.