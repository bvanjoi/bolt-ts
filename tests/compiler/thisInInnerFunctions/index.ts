// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInInnerFunctions.ts`, Apache-2.0 License

class Foo {
  x = "hello";
  bar() {
      function inner() {
          this.y = "hi"; // 'this' should be not type to 'Foo' either
          //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
          var f = () => this.y;  // 'this' should be not type to 'Foo' either
          //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
      }
  }
}

function test() {
  var x = () => {
      (() => this)();
      //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
      //~| ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
      this;
      //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
  };
}
