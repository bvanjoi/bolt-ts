// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveGenericMethodCall.ts`, Apache-2.0 License

class C {
  static g(t: typeof C.g){ }
}

C.g(C.g)
C.g((a) => {
  a(a)
})