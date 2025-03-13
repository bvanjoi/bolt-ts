// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionCall5.ts`, Apache-2.0 License

module m1 { export class c1 { public a; }} 
function foo():m1.c1{return new m1.c1();}; 
var x = foo();
x.a;