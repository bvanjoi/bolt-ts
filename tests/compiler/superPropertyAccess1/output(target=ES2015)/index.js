class C {
  foo() {}
  get x() {
    return 1;
  }
  bar() {}
}
class D extends C {
  foo() {
    super.bar();
    super.x;
  }
  constructor() {super();super.bar();
    super.x;}
  get y() {
    super.bar();
    super.x;
    return 1;
  }
}