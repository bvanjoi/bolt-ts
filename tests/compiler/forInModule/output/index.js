// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forInModule.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Foo = {};
(function (Foo) {

  for ( var i = 0; i < 1; i++) {
    i + i;
  }
  
})(Foo);