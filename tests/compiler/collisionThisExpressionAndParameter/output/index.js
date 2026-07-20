class Foo {
  x() {
    var _this = 10;
    function inner(_this) {
      return (x) => (this);
    }
  }
  y() {
    var lamda = (_this) => ((x) => (this));
  }
  z(_this) {
    var lambda = () => ((x) => (this));
  }
  x1() {
    var _this = 10;
    function inner(_this) {}
  }
  y1() {
    var lamda = (_this) => {};
  }
  z1(_this) {
    var lambda = () => {};
  }
}
class Foo1 {
  constructor(_this) {var x2 = {
          doStuff: (callback) => (() => (callback(this)))      
    };}
}

function f1(_this) {
  (x) => {
    console.log(this.x);
  };
}
class Foo3 {
  constructor(_this) {var x2 = {
          doStuff: (callback) => (() => (callback(this)))      
    };}
  z(_this) {
    var lambda = () => ((x) => (this));
  }
}

function f3(_this) {
  (x) => {
    console.log(this.x);
  };
}