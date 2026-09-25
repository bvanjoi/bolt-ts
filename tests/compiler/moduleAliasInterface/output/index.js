var _modes = {};
(function (_modes) {

  class Mode {}
  _modes.Mode = Mode;
  
})(_modes);
var editor = {};
(function (editor) {

  var modes = _modes
  
  var i;
  
  class Bug {
    constructor(p1, p2) {}
    foo(p1) {}
  }
  
})(editor);
var modesOuter = _modes
var editor2 = {};
(function (editor2) {

  var i;
  
  class Bug {
    constructor(p1, p2) {}
  }
  
  var Foo = {};
  (function (Foo) {
  
    class Bar {}
    Foo.Bar = Bar;
    
  })(Foo);
  
  class Bug2 {
    constructor(p1, p2) {}
  }
  
})(editor2);
var A1 = {};
(function (A1) {

  class A1C1 {}
  A1.A1C1 = A1C1;
  
})(A1);
var B1 = {};
(function (B1) {

  var A1Alias1 = A1
  
  var i;
  
  var c;
  
})(B1);