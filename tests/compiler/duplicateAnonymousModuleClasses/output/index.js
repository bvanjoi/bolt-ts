// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/duplicateAnonymousModuleClasses.ts`, Apache-2.0 License
var F = {};
(function (F) {

  // Should not be an error
  class Helper {}
  
})(F);

(function (F) {

  class // Should not be an error
  Helper {}
  
})(F);
var Foo = {};
(function (Foo) // Should not be an error
{

  class Helper {}
  
})(Foo);

(function (Foo) {

  class Helper {}
  
})(Foo);
var Gar = {};
(function (Gar) {

  var Foo = {};
  (function (Foo) {
  
    class Helper {}
    
  })(Foo);
  
  
  (function (Foo) {
  
    class Helper {}
    
  })(Foo);
  
})(Gar);