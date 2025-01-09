// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/errorTypesAsTypeArguments.ts`, Apache-2.0 License

interface Foo<A> {
  bar(baz: Foo<B>): Foo<C>;
  //~^ ERROR: Cannot find name 'B'.
  //~| ERROR: Cannot find name 'C'.
}