// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inOperatorWithGeneric.ts`, Apache-2.0 License

class C<T> {
  foo(x:T) {
      for (var p in x) {
      }
  }
}