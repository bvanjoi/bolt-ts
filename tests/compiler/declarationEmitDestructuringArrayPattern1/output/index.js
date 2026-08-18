// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringArrayPattern1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var [] = [1, 'hello'];
var [x] = [1, 'hello'];
var [x1, y1] = [1, 'hello'];
var [, , z1] = [0, 1, 2];
var a = [1, 'hello'];
var [x2] = a;
var [x3, y3, z3] = a;