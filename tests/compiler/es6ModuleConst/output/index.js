var a = 'hello';
var x = a, y = x;
var b = y;
var c = b, d = c;
var m1 = {};
(function (m1) {

  var k = a;
  m1.k = k
  
  var l = b, m = k;
  m1.l = l
  m1.m = m
  
  var n = m1.k;
  
  var o = n, p = k;
  
})(m1);
var m2 = {};
(function (m2) {

  var k = a;
  m2.k = k
  
  var l = b, m = k;
  m2.l = l
  m2.m = m
  
  var n = m1.k;
  
  var o = n, p = k;
  
})(m2);