// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeOfPrototype.ts`, Apache-2.0 License
class Foo {
  bar = 3
  static bar = ""
}
Foo.prototype.bar = undefined;