class A {
  m() {}
}
class B extends A {
  m() {
    try {} catch (e) {
      super.m();
    }
  }
}