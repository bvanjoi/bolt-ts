var a = {};
(function (a) {

  var x = 10;
  a.x = x
  
})(a);
var c = {};
(function (c) {

  var b = a.x
  
  var bVal = b;
  c.bVal = bVal
  
})(c);