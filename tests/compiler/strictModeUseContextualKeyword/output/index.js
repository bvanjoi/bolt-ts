'use strict';
var as = 0;
function foo(as) {}
class C {
  as() {}
}
function F() {
  function as() {}
}
function H() {
  var {as} = {
      as: 1    
  };
}