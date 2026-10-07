// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasVar.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
var a = {};
(function (a) {

  var x = 10;
  a.x = x
  
})(a);
var c = {};
(function (c) {

  var b = a.x
  
  var bVal = b;
  c.bVal = bVal
  
})(c);