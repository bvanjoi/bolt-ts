// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/instantiatedBaseTypeConstraints.ts`, Apache-2.0 License

interface Foo<T extends Foo<T, C>, C> {
  foo(bar: C): void;
}

class Bar implements Foo<Bar, string> {
  foo(bar: string): void {
  }
}

class C implements Foo<C, string> { //~ERROR: Type 'C' is not assignable to type 'Foo'.
  foo(bar: number): void {
  }
}
