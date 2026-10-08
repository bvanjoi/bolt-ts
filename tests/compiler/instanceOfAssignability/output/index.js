// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instanceOfAssignability.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
// Derived1 is assignable to, but not a subtype of, Base
class Derived1 {
  foo;
}
// Derived2 is a subtype of Base that is not assignable to Derived1
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
    // 1.5: y: Array<number>|Array<string>
    // Want: y: Array<number>|Array<string>
    var y = x;
  }
  
}
function fn2(x) {
  if (x instanceof Derived1) {
    // 1.5: y: Base
    // Want: y: Derived1
    var y = x;
  }
  
}
function fn3(x) {
  if (x instanceof Derived2) {
    // 1.5: y: Derived2
    // Want: Derived2
    var y = x;
  }
  
}
function fn4(x) {
  if (x instanceof Derived1) {
    // 1.5: y: {}
    // Want: Derived1
    var y = x;
  }
  
}
function fn5(x) {
  if (x instanceof Derived2) {
    // 1.5: y: Derived1
    // Want: ???
    var y = x;
  }
  
}
function fn6(x) {
  if (x instanceof Giraffe) {
    // 1.5: y: Derived1
    // Want: ???
    var y = x;
  }
  
}
function fn7(x) {
  if (x instanceof Array) {
    // 1.5: y: Array<number>|Array<string>
    // Want: y: Array<number>|Array<string>
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