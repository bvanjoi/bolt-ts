


// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/bestCommonTypeWithOptionalProperties.ts`, Apache-2.0 License
var x;
var y;
var z;
// All these arrays should be X[]
var b1 = [x, y, z];
var b2 = [x, z, y];
var b3 = [y, x, z];
var b4 = [y, z, x];
var b5 = [z, x, y];
var b6 = [z, y, x];
var a1 = b1;
var a2 = b2;
var a3 = b3;
var a4 = b4;
var a5 = b5;
var a6 = b6;