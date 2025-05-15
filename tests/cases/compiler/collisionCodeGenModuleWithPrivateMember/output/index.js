// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/collisionCodeGenModuleWithPrivateMember.ts`, Apache-2.0 License
var m1 = {};
(function (m1_1) {

  class m1 {}
  
  var x = new m1();
  
  class c1 {}
  m1_1.c1 = c1;
  
})(m1);
var foo = new m1.c1();