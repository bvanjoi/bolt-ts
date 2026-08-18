// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringArrayPattern5.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var [, , z] = [1, 2, 4];
var [, a, ] = [3, 4, 5];
var [, , [, b]] = [3, 5, [0, 1]];