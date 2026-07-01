class A {
  unmeasurableUsage;
}
class B extends A {
  method() {
    return '';
  }
}
class C extends B {
  marker;
}
var x = new C();
var y = x.method();