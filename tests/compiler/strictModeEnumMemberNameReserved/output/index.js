// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictModeEnumMemberNameReserved.ts`, Apache-2.0 License
'use strict';
var E = {};
(function (E) {

  E[E['static'] = 0] = 'static'
})(E);
var x1 = E.static;