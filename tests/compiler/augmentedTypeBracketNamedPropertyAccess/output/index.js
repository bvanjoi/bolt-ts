// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypeBracketNamedPropertyAccess.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var o = {};
var f = function () {};
var r1 = o['data'];
var r2 = o['functionData'];
var r3 = f['functionData'];
var r4 = f['data'];