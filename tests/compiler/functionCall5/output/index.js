// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionCall5.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var m1 = {};
(function (m1) {

  class c1 {
    a;
  }
  m1.c1 = c1;
  
})(m1);
function foo() {
  return new m1.c1();
}
;
var x = foo();
x.a;