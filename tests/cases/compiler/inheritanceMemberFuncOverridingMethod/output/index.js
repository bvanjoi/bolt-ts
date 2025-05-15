// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inheritanceMemberFuncOverridingMethod.ts`, Apache-2.0 License
class a {
  x() {
    return "10"
  }
}
class b extends a {
  x() {
    return "20"
  }
}