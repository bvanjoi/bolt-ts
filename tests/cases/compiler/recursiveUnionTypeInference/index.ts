// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveUnionTypeInference.ts`, Apache-2.0 License

interface Foo<T> {
  x: T;
}

function bar<T>(x: Foo<T> | string): T {
  return bar(x);
}
