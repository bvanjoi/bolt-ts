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