// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonGenericClassExtendingGenericClassWithAny.ts`, Apache-2.0 License

class Foo<T> {
  t: T;
  //~^ ERROR: Property 't' has no initializer and is not definitely assigned in the constructor.
}

class Bar extends Foo<any> { } // Valid
