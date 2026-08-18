// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/compoundVarDecl1.ts`, Apache-2.0 License
var Foo = {};
(function (Foo) {

  var a = 1, b = 1;
  
  a = b + 2;
  
})(Foo);
var foo = 4, bar = 5;