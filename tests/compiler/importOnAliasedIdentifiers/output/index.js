// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importOnAliasedIdentifiers.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var A = {};
(function (A) {

  var X;
  A.X = X
  
})(A);
var B = {};// Alias only for module A

(function (B) {

  var Y// Alias for both type and member A.X
   = A
  
  var Z = A.X
  
  var v = Z;
  
})(B);