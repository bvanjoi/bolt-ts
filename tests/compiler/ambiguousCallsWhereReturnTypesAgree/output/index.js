class TestClass {
  bar(x) {}
  foo(x) {
    this.bar(x);
  }
}
class TestClass2 {
  bar(x) {
    return 0
  }
  foo(x) {
    return this.bar(x)
  }
}