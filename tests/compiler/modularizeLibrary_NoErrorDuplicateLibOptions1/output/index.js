function f(x, y, z) {
  return Array.from(arguments);
}
f(1, 2, 3);
var m = new Map();
m.clear();
m.keys();
function Baz() {}
Baz.name;
function* gen() {
  var i = 0;
  while (i < 10) {
    yield i;
    i++;
  }
}
function* gen2() {
  var i = 0;
  while (i < 10) {
    yield i;
    i++;
  }
}
Math.sign(1);
var o = {
  a: 2,
  [Symbol.hasInstance](value) {
    return false;
  }  
};
o.hasOwnProperty(Symbol.hasInstance);
async function out() {
  return new Promise(function (resolve, reject) {});
}

out().then(() => {
  console.log('Yea!');
});
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