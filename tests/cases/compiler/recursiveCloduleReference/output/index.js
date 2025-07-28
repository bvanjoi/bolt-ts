var M = {};
(function (M) {

  class C {}
  M.C = C;
  
  
  (function (C_1) {
  
    var C = M.C;
    C_1.C = C
    
  })(C);
  M.C = C;
  
  ;
  
})(M);
;