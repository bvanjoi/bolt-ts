// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads24.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(bar) {
  return function () {};
}
var x = foo(5);
var x0 = x;
var y = foo('');
var y0 = y;