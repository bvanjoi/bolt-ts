class Base5 {
  x() {
    return 'BaseX';
  }
  y() {
    return 'BaseY';
  }
}
class Sub5 extends Base5 {
  x() {
    return 'SubX';
  }
}
class SubSub5 extends Sub5 {
  x() {
    return super.x();
  }
  y() {
    return super.y();
  }
}
class Base6 {
  x() {
    return 'BaseX';
  }
}
class Sub6 extends Base6 {
  y() {
    return 'SubY';
  }
}
class SubSub6 extends Sub6 {
  y() {
    return super.y();
  }
}
var results1 = new SubSub5();
var results2 = new SubSub6();
results1.x() + results1.y() + results2.y();