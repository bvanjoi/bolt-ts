// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClassStaticMethod.ts`, Apache-2.0 License
class Foo {
  static getFoo() {}
}
class Bar extends Foo {
  static getFoo() {}
}