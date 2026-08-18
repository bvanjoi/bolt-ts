// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unexportedInstanceClassVariables.ts`, Apache-2.0 License
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