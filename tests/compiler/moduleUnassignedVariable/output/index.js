// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleUnassignedVariable.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Bar = {};
(function (Bar) {

  var a = 1;
  Bar.a = a
  
  function fooA() {
    return a;
  }
  
  var b;
  Bar.b = b
  
  function fooB() {
    return b;
  }
  
})(Bar);