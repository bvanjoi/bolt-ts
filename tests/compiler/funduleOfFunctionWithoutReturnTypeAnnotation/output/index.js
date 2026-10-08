// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/funduleOfFunctionWithoutReturnTypeAnnotation.ts`, Apache-2.0 License
function fn() {
  return fn.n;
}

(function (fn) {

  var n = 1;
  fn.n = n
  
})(fn);