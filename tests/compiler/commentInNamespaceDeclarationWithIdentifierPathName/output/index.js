// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentInNamespaceDeclarationWithIdentifierPathName.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var hello = {};
(function (hello) {

  var hi // TODO, blah
  = {};
  (function (hi) {
  
    var world = {};
    (function (world) {
    
      function foo() {}
      
    })(world);
    hi.world = world;
    
  })(hi);
  hello.hi = hi;
  
})(hello);