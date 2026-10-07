// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgumentsInFunctionExpressions.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var obj = function f(a) {
  // should not error
  var x;
  return a;
};
var obj2 = function f(a) {
  // should not error
  var x;
  return a;
};