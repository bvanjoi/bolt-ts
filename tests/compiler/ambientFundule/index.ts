// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambientFundule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare function f();
declare namespace f { var x }
declare function f(x);
