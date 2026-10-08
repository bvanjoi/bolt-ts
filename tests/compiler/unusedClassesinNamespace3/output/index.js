// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedClassesinNamespace3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters
var Validation = {};
(function (Validation) {

  class c1 {}
  
  class c2 {}
  Validation.c2 = c2;
  
  var a = new c1();
  Validation.a = a
  
})(Validation);