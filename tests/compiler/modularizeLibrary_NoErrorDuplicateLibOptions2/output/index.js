// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_NoErrorDuplicateLibOptions2.ts`, Apache-2.0 License
//@compiler-options: lib=[es5,es2015,es2015.core,es2015.symbol.wellknown]
//@compiler-options: target=es6
// Using Es6 array
function f(x, y, z) {
  return Array.from(arguments);
}
f(1, 2, 3);
// no error
// Using ES6 collection
var m = new Map();
m.clear();
m.keys()// Using ES6 iterable
;
function Baz() {// Using ES6 function
}
Baz.name;
function* gen() // Using ES6 generator
{
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
// Using ES6 promise
async function out() {
  return new Promise(function (resolve, reject) {});
}

out().then(() => {
  console.log('Yea!');
});
var t = {};
// Using Es6 proxy
var p = new Proxy(t, {});
Reflect.isExtensible({// Using ES6 reflect
});
var reg = new RegExp// Using Es6 regexp
('/s');
reg.flags;
var str = 'Hello world';
// Using ES6 string
str.includes('hello', 0);
var s = Symbol(// Using ES6 symbol
);
var o1 = {
  [// Using ES6 wellknown-symbol
  Symbol.hasInstance](value) {
    return false;
  }  
};