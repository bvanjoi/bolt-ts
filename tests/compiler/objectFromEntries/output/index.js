// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectFromEntries.ts`, Apache-2.0 License
//@compiler-options: target=es2019
var o = Object.fromEntries([['a', 1], ['b', 2], ['c', 3]]);
var o2 = Object.fromEntries(new URLSearchParams());
var o3 = Object.fromEntries(new Map([[Symbol('key'), 'value']]));
var frozenArray = Object.freeze([['a', 1], ['b', 2], ['c', 3]]);
var o4 = Object.fromEntries(frozenArray);
var frozenArray2 = Object.freeze([['a', 1], ['b', 2], ['c', 3]]);
var o5 = Object.fromEntries(frozenArray2);