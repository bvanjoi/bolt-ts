class Foo {
  static {{
    console.log(this, Foo);
  }}
  static x = () => {
    console.log(this, Foo);
  };
  static y = function () {
    console.log(this, Foo);
  };
  #x() {
    console.log(Foo);
  }
  x() {
    this.#x();
  }
}
var oldFoo = Foo;
(Foo) = null;
oldFoo.x();
oldFoo.y();
new oldFoo().x();