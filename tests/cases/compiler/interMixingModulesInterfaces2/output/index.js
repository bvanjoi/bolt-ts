// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/interMixingModulesInterfaces2.ts`, Apache-2.0 License
var A = {};
(function (A) {

  
  
  var B = {};
  (function (B) {
  
    function createB() {
      return null
    }
    B.createB = createB;
    
  })(B);
  
})(A);
var x = null;