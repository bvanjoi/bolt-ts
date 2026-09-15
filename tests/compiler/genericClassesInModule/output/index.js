var Foo = {};
(function (Foo) {

  class B {}
  Foo.B = B;
  
  class A {}
  Foo.A = A;
  
})(Foo);
var a = new Foo.B();