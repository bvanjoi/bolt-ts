class A {
  static one(source, value) {
    return source
  }
  static goo() {
    return 0
  }
  static two(source) {
    return this.one(source, 42)
  }
}