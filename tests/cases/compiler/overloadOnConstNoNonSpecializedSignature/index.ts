// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/overloadOnConstNoNonSpecializedSignature.ts`, Apache-2.0 License

class C {
  x1(a: 'hi'); // error, no non-specialized signature in overload list
  x1(a: string) { }
}
