// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/lambdaExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

() => 0; // Needs to be wrapped in parens to be a valid expression (not declaration)
var y = 0;
(()=>0);
var x = 0;
