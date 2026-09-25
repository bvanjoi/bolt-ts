class C {
  foo;
  method() {
    var {a, b} = this.foo;
    !(a && b);
    a;
  }
}