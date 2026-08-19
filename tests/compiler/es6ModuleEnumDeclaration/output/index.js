var e1 = {};
(function (e1) {

  e1[e1['a'] = 0] = 'a'
  e1[e1['b'] = 0] = 'b'
  e1[e1['c'] = 0] = 'c'
})(e1);
var e2 = {};
(function (e2) {

  e2[e2['x'] = 0] = 'x'
  e2[e2['y'] = 0] = 'y'
  e2[e2['z'] = 0] = 'z'
})(e2);
var x = e1.a;
var y = e2.x;
var m1 = {};
(function (m1) {

  var e3 = {};
  (function (e3) {
  
    e3[e3['a'] = 0] = 'a'
    e3[e3['b'] = 0] = 'b'
    e3[e3['c'] = 0] = 'c'
  })(e3);
  m1.e3 = e3;
  
  var e4 = {};
  (function (e4) {
  
    e4[e4['x'] = 0] = 'x'
    e4[e4['y'] = 0] = 'y'
    e4[e4['z'] = 0] = 'z'
  })(e4);
  
  var x1 = e1.a;
  
  var y1 = e2.x;
  
  var x2 = e3.a;
  
  var y2 = e4.x;
  
})(m1);
var m2 = {};
(function (m2) {

  var e5 = {};
  (function (e5) {
  
    e5[e5['a'] = 0] = 'a'
    e5[e5['b'] = 0] = 'b'
    e5[e5['c'] = 0] = 'c'
  })(e5);
  m2.e5 = e5;
  
  var e6 = {};
  (function (e6) {
  
    e6[e6['x'] = 0] = 'x'
    e6[e6['y'] = 0] = 'y'
    e6[e6['z'] = 0] = 'z'
  })(e6);
  
  var x1 = e1.a;
  
  var y1 = e2.x;
  
  var x2 = e5.a;
  
  var y2 = e6.x;
  
  var x3 = m1.e3.a;
  
})(m2);