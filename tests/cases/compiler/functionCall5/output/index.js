var m1 = {};
(function (m1) {

  class c1 {
    a
  }
  m1.c1 = c1;
  
})(m1);
function foo() {
  return new m1.c1()
}

var x = foo();
x.a;