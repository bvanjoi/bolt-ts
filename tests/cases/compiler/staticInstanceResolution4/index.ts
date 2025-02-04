// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/staticInstanceResolution4.ts`, Apache-2.0 License

class A {
  public foo() {}
}

A.foo();
//~^ ERROR: Property 'foo' does not exist on type 'typeof A'.