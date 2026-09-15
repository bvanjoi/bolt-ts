var Outer = {};
(function (Outer) {

  var Inner = {};
  (function (Inner) {
  
    var m;
    Inner.m = m
    
  })(Inner);
  
  var f;
  Outer.f = f
  
})(Outer);