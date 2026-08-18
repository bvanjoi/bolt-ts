// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleScopingBug.ts`, Apache-2.0 License
var M = {};
(function (M) {

  var outer;
  
  function f() {
    var inner = outer;
  }
  
  class C {
    constructor() {var inner = outer;}
  }
  
  var X = {};
  (function (X) {
  
    var inner = outer;
    
  })(X);
  
})(M);