// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionExportsRequireAndFunctionInGlobalFile.ts`, Apache-2.0 License
function exports() {
  return 1;
}
function require() {
  return 'require';
}
var m3 = {};
(function (m3) {

  function exports() {
    return 1;
  }
  
  function require() {
    return 'require';
  }
  
})(m3);
var m4 = {};
(function (m4) {

  function exports() {
    return 1;
  }
  m4.exports = exports;
  
  function require() {
    return 'require';
  }
  m4.require = require;
  
})(m4);