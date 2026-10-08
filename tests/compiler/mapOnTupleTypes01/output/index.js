// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mapOnTupleTypes01.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: strictNullChecks
//@compiler-options: noImplicitAny
var mapOnLooseArrayLiteral = [1, 2, 3, 4].map((n) => (n * n));
var // Length 1
numTuple = [1];
var a = numTuple.map((x) => (x * x));
var // Length 2
numNum = [100, 100];
var strStr = ['hello', 'hello'];
var numStr = [100, 'hello'];
var b = numNum.map((n) => (n * n));
var c = strStr.map((s) => (s.charCodeAt(0)));
var d = numStr.map((x) => (x));
var numNumNum// Length 3
 = [1, 2, 3];
var e = numNumNum.map((n) => (n * n));
var // Length 4
numNumNumNum = [1, 2, 3, 4];
var f = numNumNumNum.map((n) => (n * n));
var // Length 5
numNumNumNumNum = [1, 2, 3, 4, 5];
var g = numNumNumNumNum.map((n) => (n * n));
var // Length 6
numNumNumNumNumNum = [1, 2, 3, 4, 5, 6];
var h = numNumNumNumNum.map((n) => (n * n));