// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads7.ts`, Apache-2.0 License

class foo { 
  private bar();
  private bar(foo: string);
  private bar(foo?: any){ return "foo" }
  public n() {
    var foo = this.bar();
    foo = this.bar("test");
  }
}
