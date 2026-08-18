// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedModulePrivateAccess.ts`, Apache-2.0 License
var a = {};
(function (a) {

  var x;
  
  var b = {};
  (function (b) {
  
    var y = x;
    
  })(b);
  
})(a);