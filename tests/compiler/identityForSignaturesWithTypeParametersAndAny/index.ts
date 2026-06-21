// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/identityForSignaturesWithTypeParametersAndAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var f: <T, U>(x: T, y: U) => T;
var f: <T, U>(x: any, y: any) => any;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'f' must be of type '(x: T, y: U) => T', but here has type '(x: any, y: any) => any'.
var g: <T, U>(x: T, y: U) => T;
var g: <T>(x: any, y: any) => any;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'g' must be of type '(x: T, y: U) => T', but here has type '(x: any, y: any) => any'.
var h: <T, U>(x: T, y: U) => T;
var h: (x: any, y: any) => any;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'h' must be of type '(x: T, y: U) => T', but here has type '(x: any, y: any) => any'.
var i: <T, U>(x: T, y: U) => T;
var i: <T, U>(x: any, y: string) => any;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'i' must be of type '(x: T, y: U) => T', but here has type '(x: any, y: string) => any'.
var j: <T, U>(x: T, y: U) => T;
var j: <T, U>(x: any, y: any) => string;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'j' must be of type '(x: T, y: U) => T', but here has type '(x: any, y: any) => string'.