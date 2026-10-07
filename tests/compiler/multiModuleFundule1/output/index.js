// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiModuleFundule1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function C(x) {}

(function (C) {

  var x = 1;
  C.x = x
  
})(C);

(function (C) {

  function foo() {}
  C.foo = foo;
  
})// using void returning function as constructor
(C);
var r = C(2);
var r2 = new C(2);
var r3 = C.foo();
var r4 = C.x;