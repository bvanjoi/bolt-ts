// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/cloduleWithRecursiveReference.ts`, Apache-2.0 License
var M = {};
(function (M) {

  class C {}
  M.C = C;
  
  
  (function (C_1) {
  
    var C = M.C;
    C_1.C = C
    
  })(C);
  M.C = C;
  
})(M);