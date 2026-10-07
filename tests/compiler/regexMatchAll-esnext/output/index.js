// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/regexMatchAll-esnext.ts`, Apache-2.0 License
//@compiler-options: target=esnext
var matches = /\w/g[Symbol.matchAll]('matchAll');
var array = [...matches];
var {index, input} = array[0];