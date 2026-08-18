// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithMemberClassConflict.ts`, Apache-2.0 License
var m1 = {};
(function (m1_1) {

  class m1 {}
  m1_1.m1 = m1;
  
})(m1);
var foo = new m1.m1();
var m2 = {};
(function (m2_1) {

  class m2 {}
  m2_1.m2 = m2;
  
  class _m2 {}
  m2_1._m2 = _m2;
  
})(m2);
var foo = new m2.m2();
var foo = new m2._m2();