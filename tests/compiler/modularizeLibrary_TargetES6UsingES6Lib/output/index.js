function f(x, y, z) {
  return Array.from(arguments);
}
f(1, 2, 3);
var m = new Map();
m.clear();
m.keys();
function Baz() {}
Baz.name;
Math.sign(1);
var o = {
  a: 2,
  [Symbol.hasInstance](value) {
    return false;
  }  
};
o.hasOwnProperty(Symbol.hasInstance);
var t = {};
var p = new Proxy(t, {});
Reflect.isExtensible({});
var reg = new RegExp('/s');
reg.flags;
var str = 'Hello world';
str.includes('hello', 0);
var s = Symbol();
var o1 = {
  [Symbol.hasInstance](value) {
    return false;
  }  
};