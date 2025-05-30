// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/instantiatedBaseTypeConstraints.ts`, Apache-2.0 License

interface Foo<T extends Foo<T, C>, C> {
  foo(bar: C): void;
}

class Bar implements Foo<Bar, string> {
  foo(bar: string): void {
  }
}

class C implements Foo<C, string> { //~ERROR: Type 'C' is not assignable to type 'Foo<C, string>'.
  foo(bar: number): void {
  //~^ ERROR: Type '(bar: number) => void' is not assignable to type '(bar: string) => void'.
  }
}
