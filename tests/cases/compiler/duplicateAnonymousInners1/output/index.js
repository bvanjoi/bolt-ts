// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateAnonymousInners1.ts`, Apache-2.0 License
var Foo = {};
(function (Foo) {

  class Helper {}
  
  class Inner {}
  
  // Inner should show up in intellisense
  var Outer = 0;
  Foo.Outer = Outer
  
})(Foo);

(function (Foo) {

  // Should not be an error
  class Helper {}
  
})(Foo);