// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/nestedGenerics.ts`, Apache-2.0 License

interface Foo<T> {
	t: T;
}

var f: Foo<Foo<number>>;

var g: Foo<Foo<string>> = {
  t: {
    t: 42 //~ ERROR: Type 'number' is not assignable to type 'string'.
  }
}