// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericConstructInvocationWithNoTypeArg.ts`, Apache-2.0 License

interface Foo<T> {
  new (x: number): Foo<T>;
}
var f2: Foo<number> = new Foo(3);
//~^ ERROR: Cannot find name 'Foo'.