class A {
  foo() {
    return 1
  }
}
class C {
  foo() {
    return 1
  }
}
class C2 extends A {}
var c;
var c2;
c = c2;
c2 = c;