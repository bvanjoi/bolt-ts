class Foo {
  x = 'hello'
  bar() {
    this.x;
    var f = () => (this.x);
  }
}
function myFn(a) {}
class myCls {
  constructor() {myFn(() => {
      myFn(() => {
        var x = this;
      });
    });}
}