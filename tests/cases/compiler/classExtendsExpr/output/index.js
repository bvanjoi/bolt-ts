{
  function f() {
    return class {}
  }
  class C extends f() {}
}
{
  class A {}
  A;
}