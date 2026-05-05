// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionExpressionAndLambdaMatchesFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class CDoc {
        constructor() {
        function doSomething(a: Function) {
        }
        doSomething(() => undefined);
        doSomething(function () { });
    }
}
