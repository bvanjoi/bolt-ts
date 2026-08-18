// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationFunctionTypeNonlocalShouldNotBeAnError.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var foo = {};
(function (foo) {

  function bar() {}
  
  var obj = {
      bar    
  };
  foo.obj = obj
  
})(foo);