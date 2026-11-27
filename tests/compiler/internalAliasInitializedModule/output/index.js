// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasInitializedModule.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
var a = {};
(function (a) {

  var b = {};
  (function (b) {
  
    class c {}
    b.c = c;
    
  })(b);
  a.b = b;
  
})(a);
var c = {};
(function (c) {

  var b = a.b
  
  var x = new b.c();
  c.x = x
  
})(c);