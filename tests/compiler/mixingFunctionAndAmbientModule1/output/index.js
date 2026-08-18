// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mixingFunctionAndAmbientModule1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var A = {};
(function (A) {

  
  
  function My(s) {}
  
})(A);
var B = {};
(function (B) {

  
  
  function My(s) {}
  
})(B);
var C = {};
(function (C) {

  
  
})(C);
var D = {};
(function (D) {

  
  
})(D);
var E = {};
(function (E) {

  
  
  
  
})(E);