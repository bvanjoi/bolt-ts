// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constEnumOnlyModuleMerging.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Outer = {};
(function (Outer) {

  var x = 1;
  Outer.x = x
  
})(Outer);

(function (Outer) {

  var A = {};
  (function (A) {
  
    A[A['X'] = 0] = 'X'
  })(A);
  Outer.A = A;
  
})(Outer);
var B = {};
(function (B) {

  var O = Outer
  
  var x = O.A.X;
  
  var y = O.x;
  
})(B);