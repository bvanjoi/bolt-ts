// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonGenericClassExtendingGenericClassWithAny.ts`, Apache-2.0 License

class Foo<T> {
  t: T;
}

class Bar extends Foo<any> { } // Valid