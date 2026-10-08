// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateAnonymousInners1.ts`, Apache-2.0 License
var Foo = {};
(function (Foo) {

  // Inner should show up in intellisense
  class Helper {}
  
  class Inner {}
  
  var Outer // Should not be an error
  = 0;
  Foo.// Inner should not show up in intellisense
  // Outer should show up in intellisense
  Outer = Outer
  
})(Foo);

(function (Foo) {

  class Helper {}
  
})(Foo);