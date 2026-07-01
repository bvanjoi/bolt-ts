class SomeAbstractClass extends SomeBaseClass {
  foo;
  bar;
}
class SomeClass extends SomeAbstractClass {
  baz(context) {
    return `${context}`;
  }
}