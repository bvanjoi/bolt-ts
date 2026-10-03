class Derived1 {
  foo;
}
class Derived2 {
  foo;
  optional;
}
class Animal {
  move;
}
class Mammal extends Animal {
  milk;
}
class Giraffe extends Mammal {
  neck;
}
function fn1(x) {
  if (x instanceof Array) {
    var y = x;
  }
  
}
function fn2(x) {
  if (x instanceof Derived1) {
    var y = x;
  }
  
}
function fn3(x) {
  if (x instanceof Derived2) {
    var y = x;
  }
  
}
function fn4(x) {
  if (x instanceof Derived1) {
    var y = x;
  }
  
}
function fn5(x) {
  if (x instanceof Derived2) {
    var y = x;
  }
  
}
function fn6(x) {
  if (x instanceof Giraffe) {
    var y = x;
  }
  
}
function fn7(x) {
  if (x instanceof Array) {
    var y = x;
  }
  
}
class ABC {
  a;
  b;
  c;
}
function fn8(x) {
  if (x instanceof ABC) {
    var y = x;
  }
  
}