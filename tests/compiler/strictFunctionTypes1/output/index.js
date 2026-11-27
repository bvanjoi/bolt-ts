// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictFunctionTypes1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@run-fail
var x1 = f1(fo, fs);// (x: string) => void

var x2 = f2('abc', fo, fs);// "abc"

var x3 = f3('abc', fo, fx);// "abc" | "def"

var x4 = f4(fo, fs);// Func<string>


var x10 = f2(never, fo, fs);
var x11 = f3(never// string
, fo, fx);// "def"
// Repro from #21112

var x = foo([]);// never
// Modified repros from #26127



var t1 = coAndContra(a, acceptUnion);
var t2 = coAndContra(b, acceptA);
var t3 = coAndContra(never, acceptA);
var t4 = coAndContraArray([a], acceptUnion);
var t5 = coAndContraArray([b], acceptA);
var t6 = coAndContraArray([], acceptA);