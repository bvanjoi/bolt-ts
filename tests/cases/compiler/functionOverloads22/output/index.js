// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads22.ts`, Apache-2.0 License


function foo(bar) {
  return [{a: ""}]
}
var a = foo(5);
var b = foo("");