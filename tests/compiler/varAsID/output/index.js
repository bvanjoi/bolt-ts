// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/varAsID.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Foo {
  var;
  x = 1;
}
var f = new Foo();
class Foo2 {
  var;
  x = 1;
}
var f2 = new Foo2();