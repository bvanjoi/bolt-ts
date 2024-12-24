// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/inheritanceStaticAccessorOverridingProperty.ts`, Apache-2.0 License

class a {
  static x: string;
}

class b extends a {
  static get x() {
      return "20";
  }
  static set x(aValue: string) {

  }
}