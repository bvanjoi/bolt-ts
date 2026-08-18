// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClassImplementingGenericInterfaceFromAnotherModule.ts`, Apache-2.0 License
//@compiler-options: target=es2015

var bar = {};
(function (bar) {

  class Foo {}
  bar.Foo = Foo;
  
})(bar);