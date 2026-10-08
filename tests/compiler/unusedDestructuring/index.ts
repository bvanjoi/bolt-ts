// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedDestructuring.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

export {};
declare const o: any;
const { a, b } = o;
//~^ ERROR: All destructured elements are unused.
const { c, d }  = o;
//~^ ERROR: 'c' is declared but its value is never read.
d;
const { e } = o;
//~^ ERROR: 'e' is declared but its value is never read.
const { f: g } = o;
//~^ ERROR: 'g' is declared but its value is never read.
const { h } = o, { i } = o;
//~^ ERROR: All variables are unused.

function f({ a, b }, { c, d }, { e }) {
  //~^ ERROR: 'f' is declared but its value is never read.
  //~| ERROR: All destructured elements are unused.
  //~| ERROR: 'c' is declared but its value is never read.
  //~| ERROR: 'e' is declared but its value is never read.
    d;
}

const { e: {e0} } = o;
//~^ ERROR: 'e0' is declared but its value is never read.
const { e: [e1] } = o;
//~^ ERROR: 'e1' is declared but its value is never read.
