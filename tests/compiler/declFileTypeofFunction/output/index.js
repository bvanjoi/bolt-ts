// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeofFunction.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration
function f() {
  return undefined;
}
function g() {
  return undefined;
}
var b;
function b1() {
  return b1;
}
function foo() {
  return null;
}
var foo1;
var foo2 = foo;
var foo3 = function () {
  return foo3;
};
var x = () => (x);
function foo5(x) {
  function bar(x) {
    return x;
  }
  return bar;
}