// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/structural1.ts`, Apache-2.0 License
var M = {};
(function (M) {

  
  
  function f(i) {}
  M.f = f;
  
  f({salt: 2,
  pepper: 0});
  
})(M);