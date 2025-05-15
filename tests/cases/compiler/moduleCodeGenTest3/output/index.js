// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/moduleCodeGenTest3.ts`, Apache-2.0 License
var Baz = {};
(function (Baz) {

  var x = "hello";
  Baz.x = x
  
})(Baz);
Baz.x = "goodbye";