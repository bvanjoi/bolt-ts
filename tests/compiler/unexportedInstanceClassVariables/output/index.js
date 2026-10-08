// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unexportedInstanceClassVariables.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  class A {
    constructor(val) {}
  }
  
})(M);

(function (M) {

  class A {}
  
  var a = new A();
  
})(M);