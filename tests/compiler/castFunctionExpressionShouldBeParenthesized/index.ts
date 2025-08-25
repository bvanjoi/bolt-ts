// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/castFunctionExpressionShouldBeParenthesized.ts`, Apache-2.0 License

//@ run-fail

(function a() { } as any)().foo()