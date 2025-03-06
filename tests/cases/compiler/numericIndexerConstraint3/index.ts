// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/numericIndexerConstraint3.ts`, Apache-2.0 License

class A {
  foo: number;
}

class B extends A {
  bar: string;
}

class C {
  0: B;
  [x: number]: A;
}