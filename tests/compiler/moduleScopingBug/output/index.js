// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleScopingBug.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M = {};
(function (M) {

  var outer;
  
  function f() // Ok
  {
    var inner = outer;
  }
  
  class C {
    constructor() // Ok
    {var inner = outer;}
  }
  
  var X // Error: outer not visible
  = {};
  (function (X) {
  
    var inner = outer;
    
  })(X);
  
})(M);