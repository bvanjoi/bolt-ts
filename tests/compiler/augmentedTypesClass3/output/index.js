// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesClass3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class c5 {
  foo() {}
}

class c5a {
  foo() {}
}

(function (c5a) {

  var y = 2;
  
})(c5a);
class c5b {
  foo() {}
}

(function (c5b) {

  var y = 2;
  c5b.y = y
  
})(c5b);
class c5c {
  foo() {}
}