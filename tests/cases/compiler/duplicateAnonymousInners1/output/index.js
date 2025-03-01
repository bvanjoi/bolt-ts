var Foo = {};
(function (Foo) {

  class Helper {}
  
  class Inner {}
  
  var Outer = 0;
  Foo.Outer = Outer
  
})(Foo);

(function (Foo) {

  class Helper {}
  
})(Foo);