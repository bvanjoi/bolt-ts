// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/thisInOuterClassBody.ts`, Apache-2.0 License

class Foo {

  x = this;

  static y = this;

  bar() {

      this.x; // 'this' is type 'Foo'

      var f = () => this.x; // 'this' should be type 'Foo' as well
      var p = this.y;
      //~^ ERROR: Property 'y' does not exist on type 'Foo'.
      return this;
  }

  static bar2() {
      var a = this.y;
      var b = this.x;
      //~^ ERROR: Property 'x' does not exist on type 'typeof Foo'.
  }
}