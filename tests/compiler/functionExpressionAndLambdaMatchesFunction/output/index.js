// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionExpressionAndLambdaMatchesFunction.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class CDoc {
  constructor() {function doSomething(a) {}
    doSomething(() => (undefined));
    doSomething(function () {});}
}