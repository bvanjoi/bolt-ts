// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionWithNoBestCommonType2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var v = function () {
  return true;
  return bar();
};
function bar() {}