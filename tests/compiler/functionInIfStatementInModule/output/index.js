// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionInIfStatementInModule.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Midori = {};
(function (Midori) {

  if (false) {
    function Foo(src) {}
  }
  
  
})(Midori);