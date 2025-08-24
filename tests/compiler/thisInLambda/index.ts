// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInLambda.ts`, Apache-2.0 License

class Foo {
  x = "hello";
  bar() {
      this.x; // 'this' is type 'Foo'
      var f = () => this.x; // 'this' should be type 'Foo' as well
  }
}

function myFn(a:any) { }
class myCls {
  constructor () {
      myFn(() => {
          myFn(() => {
              var x = this;
          });
      });
  }
}