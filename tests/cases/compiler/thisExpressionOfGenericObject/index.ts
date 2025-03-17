// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisExpressionOfGenericObject.ts`, Apache-2.0 License

class MyClass1<T> {
  private obj: MyClass1<string>;
  constructor() {
      () => this;
  }
}
