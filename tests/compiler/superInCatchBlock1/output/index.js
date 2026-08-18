// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superInCatchBlock1.ts`, Apache-2.0 License
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