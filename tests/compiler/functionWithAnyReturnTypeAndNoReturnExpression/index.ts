// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionWithAnyReturnTypeAndNoReturnExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f(): any { }
var f2: () => any = () => { };
var f3 = (): any => { };