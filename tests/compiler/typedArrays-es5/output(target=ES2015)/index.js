// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typedArrays-es6.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@[target=ES2015]  run-fail
var float32Array = new Float32Array(1);
[...float32Array];
//~[target=ES5]^ ERROR: Type 'Float32Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var float64Array = new Float64Array(1);
[...float64Array];
//~[target=ES5]^ ERROR: Type 'Float64Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var int16Array = new Int16Array(1);
[...int16Array];
//~[target=ES5]^ ERROR: Type 'Int16Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var int32Array = new Int32Array(1);
[...int32Array];
//~[target=ES5]^ ERROR: Type 'Int32Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var int8Array = new Int8Array(1);
[...int8Array];
//~[target=ES5]^ ERROR: Type 'Int8Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var nodeList = new NodeList();
[...nodeList];
//~[target=ES5]^ ERROR: Type 'NodeList' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var uint16Array = new Uint16Array(1);
[...uint16Array];
//~[target=ES5]^ ERROR: Type 'Uint16Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var uint32Array = new Uint32Array(1);
[...uint32Array];
//~[target=ES5]^ ERROR: Type 'Uint32Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var uint8Array = new Uint8Array(1);
[...uint8Array];
//~[target=ES5]^ ERROR: Type 'Uint8Array<ArrayBuffer>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
var uint8ClampedArray = new Uint8ClampedArray(1);
[...uint8ClampedArray];