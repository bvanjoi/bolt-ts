// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByInstanceof.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
function foo(x, A, B, AB) {
  if (x instanceof A) {
    x;// A
    
  } else {
    x;// B | C
    
  }
  
  if (x instanceof B) {
    x;// B
    
  } else {
    x;// A | C
    
  }
  
  if (x instanceof AB) {
    x;// A | B
    
  } else {
    x;// A | B | C
    
  }
  
}
function bar(target, Promise) {
  if (target instanceof Promise) {
    target.__then();
  }
  
}
// Repro from #52571
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