// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/downlevelLetConst12.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

'use strict'
// top level let\const should not be renamed
let foo;
const bar = 1;

let [baz] = [];
//~^ ERROR: Tuple type '[]' of length '0' has no element at index '0'.
let {a: baz2} = { a: 1 };

const [baz3] = []
//~^ ERROR: Tuple type '[]' of length '0' has no element at index '0'.
const {a: baz4} = { a: 1 };