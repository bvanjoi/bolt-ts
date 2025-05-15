// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads7.ts`, Apache-2.0 License
class foo {
  
  
  bar(foo) {
    return "foo"
  }
  n() {
    var foo = this.bar();
    foo = this.bar("test");
  }
}