// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/regexMatchAll.ts`, Apache-2.0 License
var matches = /\w/g[Symbol.matchAll]('matchAll');
var array = [...matches];
var {index, input} = array[0];