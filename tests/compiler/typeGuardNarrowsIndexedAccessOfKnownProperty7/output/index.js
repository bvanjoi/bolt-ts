var Foo = {};
(function (Foo) {

  var key = Symbol();
  Foo.key = key
  
})(Foo);
class C {
  [Foo.key];
  constructor() {this[Foo.key] = 'hello';}
}