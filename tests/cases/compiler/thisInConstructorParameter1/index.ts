// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInConstructorParameter1.ts`, Apache-2.0 License

class Foo {
  public y;
  constructor(x = this.y) { }
}