// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedModulePrivateAccess.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var a = {};
(function (a) {

  var x;
  
  var b// should not be an error
   = {};
  (function (b) {
  
    var y = x;
    
  })(b);
  
})(a);