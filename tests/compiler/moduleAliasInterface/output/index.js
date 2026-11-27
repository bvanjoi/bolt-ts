// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleAliasInterface.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var _modes = {};
(function (_modes) {

  class Mode {// _modes. // produces an internal error - please implement in derived class
  }
  _modes.Mode = Mode;
  
})(_modes);
var editor = // If you just use p1:modes, the compiler accepts it - should be an error
{};
(function (editor) {

  var modes = _modes
  
  var i;
  
  class Bug {
    constructor// should be an error on p2 - it's not exported
    (p1, p2) {}
    foo(p1) {}
  }
  
})(editor);
var modesOuter = _modes
var editor2 = {};
(function (editor2) {

  var i;
  
  class Bug {
    constructor(p1, p2)// no error here, since modesOuter is declared externally
     {}
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