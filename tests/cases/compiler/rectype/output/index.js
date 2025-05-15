// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/rectype.ts`, Apache-2.0 License
var M = {};
(function (M) {

  
  
  function f(p) {
    return f
  }
  M.f = f;
  
  
  
  var i;
  
  f(i);
  
  f(f(i));
  
  f((f(f(i))));
  
})(M);