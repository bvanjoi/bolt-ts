// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisExpressionInIndexExpression.ts`, Apache-2.0 License

function f() {
  return r => r[this];
}