// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/numericIndexerConstraint3.ts`, Apache-2.0 License

class A {
  foo: number;
  //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
}

class B extends A {
  bar: string;
  //~^ ERROR: Property 'bar' has no initializer and is not definitely assigned in the constructor.
}

class C {
  0: B;
  [x: number]: A;
}
