// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericInstanceOf.ts`, Apache-2.0 License

interface F {
  (): number;
}

class C<T> {
  constructor(public a: T, public b: F) {}
  foo() {
      if (this.a instanceof this.b) {
      }
  }
}