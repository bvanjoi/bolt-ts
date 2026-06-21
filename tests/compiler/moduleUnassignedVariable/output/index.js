var Bar = {};
(function (Bar) {

  var a = 1;
  Bar.a = a
  
  function fooA() {
    return a
  }
  
  var b;
  Bar.b = b
  
  function fooB() {
    return b
  }
  
})(Bar);