class C1 {
  func(param) {}
}
class C2 extends C1 {}
var x = new C2();
x.func(new C1());
x.func(new C2());