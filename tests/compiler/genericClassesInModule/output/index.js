// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericClassesInModule.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Foo = {};
(function (Foo) {

  class B {}
  Foo.B = B;
  
  class A {}
  Foo.A = A;
  
})(Foo);
var a = new Foo.B();