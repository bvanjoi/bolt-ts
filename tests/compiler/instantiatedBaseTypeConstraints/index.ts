// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/instantiatedBaseTypeConstraints.ts`, Apache-2.0 License

interface Foo<T extends Foo<T, C>, C> {
  foo(bar: C): void;
}

class Bar implements Foo<Bar, string> {
  foo(bar: string): void {
  }
}

class C implements Foo<C, string> { //~ERROR: Type 'C' does not satisfy the constraint 'Foo<C, string>'.
  foo(bar: number): void {
  //~^ ERROR: Property 'foo' in type 'C<C>' is not assignable to the same property in base type 'Foo<C, string, C>'.
  }
}
