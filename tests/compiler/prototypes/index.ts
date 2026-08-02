// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/prototypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

Object.prototype; // ok
new Object().prototype; // error
//~^ ERROR: Property 'prototype' does not exist on type 'Object'.
function f() {}
f.prototype;