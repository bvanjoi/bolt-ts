var a = {};
(function (a) {

  class c {}
  a.c = c;
  
})(a);
var c = {};
(function (c) {

  var b = a.c
  
  var x = new b();
  c.x = x
  
})(c);