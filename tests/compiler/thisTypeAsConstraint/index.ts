// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/thisTypeAsConstraint.ts`, Apache-2.0 License

class C {
  public m<T extends this>() {
  }
}