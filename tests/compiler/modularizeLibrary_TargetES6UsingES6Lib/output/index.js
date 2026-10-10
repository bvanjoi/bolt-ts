// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_TargetES6UsingES6Lib.ts`, Apache-2.0 License
//@compiler-options: lib=[es6]
//@compiler-options: target=es6
// Using Es6 array
function f(x, y, z) {
  return Array.from(arguments);
}
f(1, 2, 3);// no error

// Using ES6 collection
var m = new Map();
m.clear();
m.keys()// Using ES6 iterable
;
function Baz() {// Using ES6 function
}
Baz.name;
Math.sign(1// Using ES6 math
);
var o = {
  a// Using ES6 object
  : 2,
  [Symbol.hasInstance](value) {
    return false;
  }  
};
o.hasOwnProperty(Symbol.hasInstance);
// Using Es6 proxy
var t = {};
var p = new Proxy(t, {});
// Using ES6 reflect
Reflect.isExtensible({});
// Using Es6 regexp
var reg = new RegExp('/s');
reg.flags;
// Using ES6 string
var str = 'Hello world';
str.includes('hello', 0);
// Using ES6 symbol
var s = Symbol();
// Using ES6 wellknown-symbol
var o1 = {
  [Symbol.hasInstance](value) {
    return false;
  }  
};