// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticGetter2.ts`, Apache-2.0 License
class C {
  static x() {
    var r = this;
    return this
  }
}