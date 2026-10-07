// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasClass.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
var a = {};
(function (a) {

  class c {}
  a.c = c;
  
})(a);
var c = {};
(function (c) {

  var b = a.c
  
  var x = new b();
  c.x = x
  
})(c);