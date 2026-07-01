class C {
  constructor(x) {}
  foo() {}
  bar() {}
  static boo() {}
}

(function (C) {

  var x = 1;
  C.x = x
  
  var y = 2;
  
})(C);

(function (C) {

  function foo() {}
  C.foo = foo;
  
  function baz() {
    return '';
  }
  
})(C);
var c = new C(C.x);
c.foo = C.foo;