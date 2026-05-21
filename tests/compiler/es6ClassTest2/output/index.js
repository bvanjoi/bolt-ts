class BasicMonster {
  constructor(name, health) {
    this.name = name
    
    this.health = health}
  attack(target) {}
  isAlive = true;
}
var m1 = new BasicMonster('1', 100);
var m2 = new BasicMonster('2', 100);
m1.attack(m2);
m1.health = 0;
console.log((m5.isAlive).toString());
class GetSetMonster {
  constructor(name, _health) {
    this.name = name
    }
  attack(target) {}
  get isAlive() {
    return this._health > 0
  }
  set health(value) {
    if (value < 0) {
      throw new Error('Health must be non-negative.')
    }
    
    this._health = value;
  }
}
var m3 = new BasicMonster('1', 100);
var m4 = new BasicMonster('2', 100);
m3.attack(m4);
m3.health = 0;
var x = (m5.isAlive).toString();
class OverloadedMonster {
  constructor(name, health) {
    this.name = name
    
    this.health = health}
  attack(target) {}
  isAlive = true;
}
var m5 = new OverloadedMonster('1');
var m6 = new OverloadedMonster('2');
m5.attack(m6);
m5.health = 0;
var y = (m5.isAlive).toString();
class SplatMonster {
  constructor(...args) {}
  roar(name, ...args) {}
}
function foo() {
  return true
}
class PrototypeMonster {
  age = 1;
  name;
  b = foo();
}
class SuperParent {
  constructor(a) {}
  b(b) {}
  c() {}
}
class SuperChild extends SuperParent {
  constructor() {super(1);}
  b() {
    super.b('str');
  }
  c() {
    super.c();
  }
}
class Statics {
  static foo = 1;
  static bar;
  static baz() {
    return ''
  }
}
var stat = new Statics();
class ImplementsInterface {
  x;
  z;
  constructor() {this.x = 1;
    this.z = 'foo';}
}
class Visibility {
  foo() {}
  bar() {}
  x;
  y;
  z;
  constructor() {this.x = 1;
    this.y = 2;}
}
class BaseClassWithConstructor {
  constructor(x, s) {
    this.x = x
    
    this.s = s}
}
class ChildClassWithoutConstructor extends BaseClassWithConstructor {}
var ccwc = new ChildClassWithoutConstructor(1, 's');