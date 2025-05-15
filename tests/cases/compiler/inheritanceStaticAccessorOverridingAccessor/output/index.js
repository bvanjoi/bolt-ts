// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inheritanceStaticAccessorOverridingAccessor.ts`, Apache-2.0 License
class a {
  static get x() {
    return "20"
  }
  static set x(aValue) {}
}
class b extends a {
  static get x() {
    return "20"
  }
  static set x(aValue) {}
}