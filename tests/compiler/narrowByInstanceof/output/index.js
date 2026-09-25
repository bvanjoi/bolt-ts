function foo(x, A, B, AB) {
  if (x instanceof A) {
    x;
  } else {
    x;
  }
  
  if (x instanceof B) {
    x;
  } else {
    x;
  }
  
  if (x instanceof AB) {
    x;
  } else {
    x;
  }
  
}
function bar(target, Promise) {
  if (target instanceof Promise) {
    target.__then();
  }
  
}
class PersonMixin extends Function {
  check(o) {
    return typeof o === 'object' && o !== null && o instanceof Person;
  }
}
var cls = new PersonMixin();
class Person {
  work() {
    console.log('work');
  }
  sayHi() {
    console.log('Hi');
  }
}
class Car {
  sayHi() {
    console.log('Wof Wof');
  }
}
function test(o) {
  if (o instanceof cls) {
    console.log('Is Person');
    (o).work();
  } else {
    console.log('Is Car');
    o.sayHi();
  }
  
}