// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads33.ts`, Apache-2.0 License


function foo(bar) {
  return bar
}
var x = foo(5);
var y = foo(5);
var z = foo("s");