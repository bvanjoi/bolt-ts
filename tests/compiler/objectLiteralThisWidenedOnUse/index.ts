// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralThisWidenedOnUse.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noImplicitThis

interface Foo { bar: boolean; }

var GlobalIns = {
  prop1: 1,
  prop2: 2,
  prop3: 3,
  test () {
    this.accept_foo(this);
    //~^ ERROR: Property 'bar' is missing.
  },
  accept_foo (foo: Foo): boolean {
    return !!foo && !!foo.bar;
  }
};