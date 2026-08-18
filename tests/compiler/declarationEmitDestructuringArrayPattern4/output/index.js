// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringArrayPattern4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var [...a5] = [1, 2, 3];
var [x14, ...a6] = [1, 2, 3];
var [x15, y15, ...a7] = [1, 2, 3];
var [x16, y16, z16, ...a8] = [1, 2, 3];
var [...a9] = [1, 'hello', true];
var [x17, ...a10] = [1, 'hello', true];
var [x18, y18, ...a12] = [1, 'hello', true];
var [x19, y19, z19, ...a13] = [1, 'hello', true];