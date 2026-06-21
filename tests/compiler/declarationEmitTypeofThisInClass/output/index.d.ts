declare class Foo {
  foo: string;
  bar: typeof this.foo;
  baz: typeof this;
}
