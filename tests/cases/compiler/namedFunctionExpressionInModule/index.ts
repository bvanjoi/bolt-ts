// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/namedFunctionExpressionInModule.ts`, Apache-2.0 License

module Variables{
  var x = function bar(a, b, c) {
  }
  x(1, 2, 3);
}
