// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/collisionCodeGenModuleWithMemberVariable.ts`, Apache-2.0 License
var m1 = {};
(function (m1_1) {

  var m1 = 10;
  m1_1.m1 = m1
  
  var b = m1;
  
})(m1);
var foo = m1.m1;