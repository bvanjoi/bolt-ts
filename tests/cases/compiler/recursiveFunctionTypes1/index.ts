// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveGenericMethodCall.ts`, Apache-2.0 License

class C {
  static g(t: typeof C.g){ }
}