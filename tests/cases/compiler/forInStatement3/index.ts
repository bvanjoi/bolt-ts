// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/forInStatement3.ts`, Apache-2.0 License

function F<T>() {
  var expr: T;
  for (var a in expr) {
  }
}