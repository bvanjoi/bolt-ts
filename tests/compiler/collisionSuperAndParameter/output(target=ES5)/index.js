class Foo {
  a() {
    var lamda = (_super) => ((x) => (this));
  }
  b(_super) {
    var lambda = () => ((x) => (this));
  }
  set c(_super) {}
}
class Foo2 extends Foo {
  x() {
    var lamda = (_super) => ((x) => (this));
  }
  y(_super) {
    var lambda = () => ((x) => (this));
  }
  set z(_super) {}
  prop3;
  prop4 = {
      doStuff: (_super) => {}    
  };
  constructor(_super) {super();}
}
class Foo4 extends Foo {
  constructor(_super) {super();}
  y(_super) {
    var lambda = () => ((x) => (this));
  }
}