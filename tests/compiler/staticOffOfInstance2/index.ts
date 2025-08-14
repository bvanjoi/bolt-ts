// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticOffOfInstance2.ts`, Apache-2.0 License

class List<T> {
  public Blah() {
      this.Foo(); // no error
      //~^ ERROR: Property 'Foo' does not exist on type 'List'.
      List.Foo();
  }
  public static Foo() { }
}
