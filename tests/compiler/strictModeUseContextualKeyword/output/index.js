// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictModeUseContextualKeyword.ts`, Apache-2.0 License
'use strict';
var as = 0;
function foo(as) {}
class C {
  as() {}
}
function F() {
  function as() {}
}
function H() {
  var {as} = {
      as: 1    
  };
}