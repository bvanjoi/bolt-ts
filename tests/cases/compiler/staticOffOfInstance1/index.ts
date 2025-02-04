// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/staticOffOfInstance1.ts`, Apache-2.0 License

class List {
  public Blah() {
    this.Foo();
    //~^ ERROR: Property 'Foo' does not exist on type 'List'.
  }
  public static Foo() {}
}