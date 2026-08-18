// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionExportsRequireAndInternalModuleAliasInGlobalFile.ts`, Apache-2.0 License
var mOfGloalFile = {};
(function (mOfGloalFile) {

  class c {}
  mOfGloalFile.c = c;
  
})(mOfGloalFile);
var exports = mOfGloalFile.c
var require = mOfGloalFile.c
new exports();
new require();
var m1 = {};
(function (m1) {

  var exports = mOfGloalFile.c
  
  var require = mOfGloalFile.c
  
  new exports();
  
  new require();
  
})(m1);
var m2 = {};
(function (m2) {

  var exports = mOfGloalFile.c
  
  var require = mOfGloalFile.c
  
  new exports();
  
  new require();
  
})(m2);