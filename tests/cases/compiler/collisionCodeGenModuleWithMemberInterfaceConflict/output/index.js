// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/collisionCodeGenModuleWithMemberInterfaceConflict.ts`, Apache-2.0 License
var m1 = {};
(function (m1) {

  
  
  class m2 {}
  m1.m2 = m2;
  
})(m1);
var foo = new m1.m2();