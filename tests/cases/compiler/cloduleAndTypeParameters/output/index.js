class Foo {
  constructor() {}
}

(function (Foo) {

  
  
  class Baz {}
  Foo.Baz = Baz;
  
})(Foo);