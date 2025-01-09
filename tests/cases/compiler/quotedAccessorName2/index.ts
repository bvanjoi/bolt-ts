// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/quotedAccessorName2.ts`, Apache-2.0 License

class C {
  static get "foo"() { return 0; }
}