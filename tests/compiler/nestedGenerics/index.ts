// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nestedGenerics.ts`, Apache-2.0 License

interface Foo<T> {
	t: T;
}

var f: Foo<Foo<number>>;

var g: Foo<Foo<string>> = {
  t: {
    t: 42 //~ ERROR: Type 'number' is not assignable to type 'string'.
  }
}

const id = <T>(x: T) => x;
const nest = <T>(x: T) => id({ nested: { tag: 0, ...x } });
const eight = <T>(x: T) => nest(nest(nest(nest(nest(nest(nest(nest(x))))))));
const value = eight(eight(eight(eight(eight({ value: 0 })))));
declare function consume<T>(x: typeof value): T;
consume(value);