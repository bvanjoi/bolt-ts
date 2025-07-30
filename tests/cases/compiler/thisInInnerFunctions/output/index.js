class Foo {
  x = 'hello'
  bar() {
    function inner() {
      this.y = 'hi';
      var f = () => (this.y);
    }
  }
}
function test() {
  var x = () => {
    (() => (this))();
    this;
  };
}