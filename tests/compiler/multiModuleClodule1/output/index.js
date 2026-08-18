// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiModuleClodule1.ts`, Apache-2.0 License
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