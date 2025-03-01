// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/inheritanceStaticFuncOverridingPropertyOfFuncType.ts`, Apache-2.0 License

class a {
  static x: () => string;
}

class b extends a {
  static x() {
      return "20";
  }
}