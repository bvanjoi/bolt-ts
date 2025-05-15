// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inheritanceStaticFuncOverridingMethod.ts`, Apache-2.0 License
class a {
  static x() {
    return "10"
  }
}
class b extends a {
  static x() {
    return "20"
  }
}