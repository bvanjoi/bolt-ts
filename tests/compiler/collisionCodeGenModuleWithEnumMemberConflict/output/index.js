// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithEnumMemberConflict.ts`, Apache-2.0 License
var m1 = {};
(function (m1) {

  var e = {};
  (function (e) {
  
    e[e['m1'] = 0] = 'm1'
    e[e['m2'] = m1] = 'm2'
  })(e);
  
})(m1);