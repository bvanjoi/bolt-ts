// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/quotedAccessorName1.ts`, Apache-2.0 License

class C {
  get "foo"() { return 0; }
}