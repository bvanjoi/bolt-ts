// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionExportsRequireAndAmbientFunctionInGlobalFile.ts`, Apache-2.0 License
//@compiler-options: target=es2015

var m4 = {};
(function (m4) {

  var a = 10;
  
})(m4);