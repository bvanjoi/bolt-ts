// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads24.ts`, Apache-2.0 License


function foo(bar) {
  return function () {}
}
var x = foo(5);
var x0 = x;
var y = foo("");
var y0 = y;