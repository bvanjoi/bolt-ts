class Test {
  attrs;
  m() {
    this.attrs.params.name;
  }
}
class FooClass {
  properties;
  foo() {
    var {foo = 42} = this.properties;
    return foo;
  }
}
class Test2 {
  attrs;
  m() {
    return this.attrs.params;
  }
}