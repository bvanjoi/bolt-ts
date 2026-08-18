// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typedArrays-es6.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: target=ES6
var float32Array = new Float32Array(1);
[...float32Array];
var float64Array = new Float64Array(1);
[...float64Array];
var int16Array = new Int16Array(1);
[...int16Array];
var int32Array = new Int32Array(1);
[...int32Array];
var int8Array = new Int8Array(1);
[...int8Array];
var nodeList = new NodeList();
[...nodeList];
var uint16Array = new Uint16Array(1);
[...uint16Array];
var uint32Array = new Uint32Array(1);
[...uint32Array];
var uint8Array = new Uint8Array(1);
[...uint8Array];
var uint8ClampedArray = new Uint8ClampedArray(1);
[...uint8ClampedArray];