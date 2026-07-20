// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/implementInterfaceAnyMemberWithVoid.ts`, Apache-2.0 License

interface I {
  foo(value: number);
  //~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
}

class Bug implements I {
  public foo(value: number) {
  }
}
