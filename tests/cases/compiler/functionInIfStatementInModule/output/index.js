// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionInIfStatementInModule.ts`, Apache-2.0 License
var Midori = {};
(function (Midori) {

  if (false) {
    function Foo(src) {}
  }
  
  
})(Midori);