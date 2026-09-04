// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveLetConst.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es6

'use strict'

let x = x + 1;
//~^ ERROR: Block-scoped variable 'x' used before its declaration.
let [x1] = x1 + 1;
//~^ ERROR: Block-scoped variable 'x1' used before its declaration.
const y = y + 2;
//~^ ERROR: Block-scoped variable 'y' used before its declaration.
const [y1] = y1 + 1;
//~^ ERROR: Block-scoped variable 'y1' used before its declaration.
for (let v = v; ; ) { }
//~^ ERROR: Block-scoped variable 'v' used before its declaration.
for (let [v] = v; ;) { }
//~^ ERROR: Block-scoped variable 'v' used before its declaration.
for (let v in v) { }
//~^ ERROR: Block-scoped variable 'v' used before its declaration.
//~| ERROR: Block-scoped variable 'v' used before its declaration.
for (let v of v) { }
//~^ ERROR: Block-scoped variable 'v' used before its declaration.
for (let [v] of v) { }
//~^ ERROR: Block-scoped variable 'v' used before its declaration.
let [x2 = x2] = []
//~^ ERROR: Block-scoped variable 'x2' used before its declaration.
//~| ERROR: Block-scoped variable 'x2' used before its declaration.
let z0 = () => z0;
let z1 = function () { return z1; }
let z2 = { f() { return z2;}}