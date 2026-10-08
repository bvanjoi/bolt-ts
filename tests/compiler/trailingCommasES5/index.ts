// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/trailingCommasES5.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var o1 = { a: 1, b: 2 };
var o2 = { a: 1, b: 2, };
var o3 = { a: 1, };
var o4 = {};

var a1 = [1, 2];
var a2 = [1, 2, ];
var a3 = [1, ];
var a4 = [];
var a5 = [1, , ];
var a6 = [, , ];