// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericAssignmentCompatOfFunctionSignatures1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x1 = function foo3<T, U extends { a: T; b: string }>(x: T, z: U) { }
var x2 = function foo3<T, U extends { a: T; b: number }>(x: T, z: U) { }

x1 = x2;
//~^ ERROR: Type '<T, U>(x: T, z: U) => void' is not assignable to type '<T, U>(x: T, z: U) => void'.
x2 = x1;
//~^ ERROR: Type '<T, U>(x: T, z: U) => void' is not assignable to type '<T, U>(x: T, z: U) => void'.
