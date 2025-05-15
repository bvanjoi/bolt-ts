// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveIdenticalOverloadResolution.ts`, Apache-2.0 License
var M = {};
(function (M) {

  
  
  function f(p) {
    return f
  }
  
  
  
  var i;
  
  f(i);
  
  f(f(i));
  
  f((f(f(i))));
  
})(M);
function f0(a) {
  return f0
}

f0(f0(0));
f0(f0(f0(0)));
f0(f0(f0(f0(0))));