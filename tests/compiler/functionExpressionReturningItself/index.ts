// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionExpressionReturningItself.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var x = function somefn() { return somefn; };
