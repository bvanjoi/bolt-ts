// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/separate1-2.ts`, Apache-2.0 License
var X = {};
(function (X) {

  function f() {}
  X.f = f;
  
})(X);