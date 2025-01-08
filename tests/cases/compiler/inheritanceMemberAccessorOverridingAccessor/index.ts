// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/inheritanceMemberAccessorOverridingAccessor.ts`, Apache-2.0 License

class a {
  get x() {
      return "20";
  }
  set x(aValue: string) {

  }
}

class b extends a {
  get x() {
      return "20";
  }
  set x(aValue: string) {

  }
}