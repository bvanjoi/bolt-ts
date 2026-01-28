// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/identityForSignaturesWithTypeParametersSwitched.ts`, Apache-2.0 License

var f: <T, U>(x: T, y: U) => T;
var f: <T, U>(x: U, y: T) => U;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'f' must be of type '(x: T, y: U) => T', but here has type '(x: U, y: T) => U'.

var g: <T, U>(x: T, y: U) => T;
var g: <U, T>(x: U, y: T) => U;