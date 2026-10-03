function C(x) {}

(function (C) {

  var x = 1;
  C.x = x
  
})(C);

(function (C) {

  function foo() {}
  C.foo = foo;
  
})(C);
var r = C(2);
var r2 = new C(2);
var r3 = C.foo();
var r4 = C.x;