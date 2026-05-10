// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/implementsIncorrectlyNoAssertion.ts`, Apache-2.0 License

declare class Foo {
  x: string;
}
declare class Bar {
  y: string;
}
type Wrapper = Foo & Bar;
class Baz implements Wrapper {
  x: number;
  //~^ ERROR: Property 'x' in type 'Baz<Baz>' is not assignable to the same property in base type 'Foo<Baz> & Bar<Baz>'.
  y: string;
}
