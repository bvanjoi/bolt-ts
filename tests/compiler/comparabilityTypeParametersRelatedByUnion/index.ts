// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/comparabilityTypeParametersRelatedByUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> {
  constructor(readonly x: T) {}

  good<U extends T>(y: U) {
      if (y === this.x) {}
  }

  bad<U extends T | string>(y: U) {
      if (y === this.x) {}
  }
}