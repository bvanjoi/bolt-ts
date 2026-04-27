class m3b {
  foo() {}
}

(function (m3b) {

  var y = 2;
  
})(m3b);
class m3c {
  foo() {}
}

(function (m3c) {

  var y = 2;
  m3c.y = y
  
})(m3c);
var m3d = {};
(function (m3d) {

  var y = 2;
  m3d.y = y
  
})(m3d);
var m3e = {};
(function (m3e) {

  var y = 2;
  m3e.y = y
  
})(m3e);
var m3f = {};
(function (m3f) {

})(m3f);
var m3g = {};
(function (m3g) {

  class C {
    foo() {}
  }
  m3g.C = C;
  
})(m3g);