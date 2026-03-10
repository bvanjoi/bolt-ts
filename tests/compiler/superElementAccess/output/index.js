class MyBase {
  m1(a) {
    return a
  }
  p1() {}
  m2 = function () {}
  d1 = 42
  d2 = 42
  get value() {
    return 0
  }
  set value(v) {}
}
class MyDerived extends MyBase {
  foo() {
    super['m1']('hi');
    var l2 = super['m1'].bind(this);
    var x = super['m1'];
    super['m2'].bind(this);
    super['p1']();
    var l1 = super['d1'];
    var l1 = super['d2'];
    super['m1'] = function (a) {
      return ''
    };
    super['value'] = 0;
    var z = super['value'];
  }
}