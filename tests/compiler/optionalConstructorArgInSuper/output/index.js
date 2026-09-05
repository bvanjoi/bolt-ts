class Base {
  constructor(opt) {}
  foo(other) {}
}
class Derived extends Base {}
var d = new Derived();
var d2;
d2.foo();