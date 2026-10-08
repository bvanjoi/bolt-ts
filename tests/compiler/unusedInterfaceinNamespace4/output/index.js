// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedInterfaceinNamespace4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters
var Validation = {};
(function (Validation) {

  class c1 {}
  Validation.c1 = c1;
  
})(Validation);