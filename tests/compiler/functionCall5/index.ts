// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionCall5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

module m1 { export class c1 { public a; }} 
function foo():m1.c1{return new m1.c1();}; 
var x = foo();
x.a;
