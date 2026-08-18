// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisInConstructorParameter1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Foo {
  y;
  constructor(x = this.y) {}
}