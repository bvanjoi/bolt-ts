// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/publicGetterProtectedSetterFromThisParameter.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class A {
  get x() { return 0; }
  protected set x(v: number) { }

  public get y() { return 0; }
  protected set y(v: number) { }
}

class B {
  get q() { return 0; }
  protected set q(v: number) { }

  protected get u() { return 0; }
  protected set u(v: number) { }

  foo(this: A, a: A, b: B) {
    // Should have no errors in this block
    this.x = 0;
    this.y = 0;
    a.x = 0;
    a.y = 0;
    b.q = 0;
    b.u = 0;
  }
}

function bar(this: A, a: A, b: B) {
    this.x = 0;
    this.y = 0;
    a.x = 0;
    a.y = 0;
    // These should error
    b.q = 0;
    //~^ ERROR: Property 'q' is protected and only accessible within class 'B' and its subclasses.
    b.u = 0;
    //~^ ERROR: Property 'u' is protected and only accessible within class 'B' and its subclasses.
}
