// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classBlockScoping.ts`, Apache-2.0 License
function f(b) {
  var Foo;
  if (b) {
    Foo = class Foo {
      static y = new Foo();
      static x() {
        new Foo();
      }
      m() {
        new Foo();
      }
    };
    new Foo();
  } else {
    class Foo {
      static y = new Foo();
      static x() {
        new Foo();
      }
      m() {
        new Foo();
      }
    }
    new Foo();
  }
  
}