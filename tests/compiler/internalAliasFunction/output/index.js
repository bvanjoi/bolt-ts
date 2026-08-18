// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasFunction.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var a = {};
(function (a) {

  function foo(x) {
    return x;
  }
  a.foo = foo;
  
})(a);
var c = {};
(function (c) {

  var b = a.foo
  
  var bVal = b(10);
  c.bVal = bVal
  
  var bVal2 = b;
  c.bVal2 = bVal2
  
})(c);