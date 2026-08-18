// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/importAndVariableDeclarationConflict2.ts`, Apache-2.0 License
var m = {};
(function (m_1) {

  var m = '';
  m_1.m = m
  
})(m);
var x = m.m
class C {
  foo() {
    var x = '';
  }
}