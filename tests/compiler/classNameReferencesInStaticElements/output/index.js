// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classNameReferencesInStaticElements.ts`, Apache-2.0 License
//@compiler-options: target=es2015
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