// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisExpressionOfGenericObject.ts`, Apache-2.0 License

class MyClass1<T> {
  private obj: MyClass1<string>;
  //~^ ERROR: Property 'obj' has no initializer and is not definitely assigned in the constructor.
  constructor() {
      () => this;
  }
}
