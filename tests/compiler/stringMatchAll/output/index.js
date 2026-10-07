// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/stringMatchAll.ts`, Apache-2.0 License
//@compiler-options: target=es2020
var matches = 'matchAll'.matchAll(/\w/g);
var array = [...matches];
var {index, input} = array[0];