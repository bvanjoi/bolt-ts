// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cloduleAndTypeParameters.ts`, Apache-2.0 License
class Foo {
  constructor() {}
}

(function (Foo) {

  
  
  class Baz {}
  Foo.Baz = Baz;
  
})(Foo);