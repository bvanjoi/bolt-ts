// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/global.ts`, Apache-2.0 License
var M = {};
(function (M) {

  function f(y) {
    return x + y;
  }
  M.f = f;
  
})(M);
var x = 10;
M.f(3);