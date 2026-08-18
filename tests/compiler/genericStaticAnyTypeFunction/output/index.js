// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericStaticAnyTypeFunction.ts`, Apache-2.0 License
class A {
  static one(source, value) {
    return source;
  }
  static goo() {
    return 0;
  }
  static two(source) {
    return this.one(source, 42);
  }
}