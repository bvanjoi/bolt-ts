export function foo() {}
function foo2() {}
foo();
foo2();
var m1 = {};
(function (m1) {

  function foo3() {}
  m1.foo3 = foo3;
  
  function foo4() {}
  
  foo();
  
  foo2();
  
  foo3();
  
  foo4();
  
})(m1);
var m2 = {};
(function (m2) {

  function foo3() {}
  m2.foo3 = foo3;
  
  function foo4() {}
  
  foo();
  
  foo2();
  
  foo3();
  
  foo4();
  
  m1.foo3();
  
})(m2);