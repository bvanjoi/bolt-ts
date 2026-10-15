// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesClass3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// class then module
class c5 {
  foo() {}
}

class c5a {
  foo(// should be ok
  ) {}
}

(function (c5a) {

  var y = 2;
  
})(c5a)// should be ok
;
class c5b {
  foo() {}
}

(function (c5b) {

  var y = 2;// should be ok
  
  c5b.y = y
  
})(c5b);
class c5c {
  foo() {}
}