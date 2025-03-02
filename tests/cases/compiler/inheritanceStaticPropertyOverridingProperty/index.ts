// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inheritanceStaticPropertyOverridingProperty.ts`, Apache-2.0 License

class a {
  static x: () => string;
}

class b extends a {
  static x: () => string;
}