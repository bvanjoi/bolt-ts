// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileInternalAliases.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var m = {};
(function (m) {

  class c {}
  m.c = c;
  
})(m);
var m1 = {};
(function (m1) {

  var x = m.c
  
  var d = new x();
  m1.d = d
  
})(m1);
var m2 = {};
(function (m2) {

  var x = m.c
  
  var d = new x();
  m2.d = d
  
})(m2);