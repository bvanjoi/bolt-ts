// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_ErrorFromUsingES6FeaturesWithOnlyES5Lib.ts`, Apache-2.0 License

//@compiler-options: lib=[es5]
//@compiler-options: target=es6

// All will be error from using ES6 features but only include ES5 library
// Using Es6 array
function f(x: number, y: number, z: number) {
    return Array.from(arguments);
    //~^ ERROR: Property 'from' does not exist on type 'ArrayConstructor'.
}

f(1, 2, 3);  // no error

// Using ES6 collection
var m = new Map<string, number>();
//~^ ERROR: Cannot find name 'Map'.
m.clear();
// Using ES6 iterable
m.keys();

// Using ES6 function
function Baz() { }
Baz.name;
//~^ ERROR: Property 'name' does not exist on type '() => void'

// Using ES6 math
Math.sign(1);
//~^ ERROR: Property 'sign' does not exist on type 'Math'

// Using ES6 object
var o = {
    a: 2,
    [Symbol.hasInstance](value: any) {
//~^ ERROR: Cannot find name 'Symbol'.
        return false;
    }
};
o.hasOwnProperty(Symbol.hasInstance);
//~^ ERROR: Cannot find name 'Symbol'.

// Using Es6 proxy
var t = {}
var p = new Proxy(t, {});
//~^ ERROR: Cannot find name 'Proxy'.

// Using ES6 reflect
Reflect.isExtensible({});
//~^ ERROR: Cannot find name 'Reflect'.

// Using Es6 regexp
var reg = new RegExp("/s");
reg.flags;
//~^ ERROR: Property 'flags' does not exist on type 'RegExp'.

// Using ES6 string
var str = "Hello world";
str.includes("hello", 0);
//~^ ERROR: Property 'includes' does not exist on type 'string'.

// Using ES6 symbol
var s = Symbol();
//~^ ERROR: Cannot find name 'Symbol'.

// Using ES6 wellknown-symbol
const o1 = {
    [Symbol.hasInstance](value: any) {
//~^ ERROR: Cannot find name 'Symbol'.
        return false;
    }
}