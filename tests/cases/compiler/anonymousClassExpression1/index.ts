// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/anonymousClassExpression1.ts`, Apache-2.0 License

function f() {
  return typeof class {} === "function";
}