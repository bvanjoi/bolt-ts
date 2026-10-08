// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyFunctionExpressionAssignment.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
var x = function (x) {
  return null;
};
var x2 = function f(x) {
  return null;
};