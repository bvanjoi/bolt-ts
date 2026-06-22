// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/regexMatchAll.ts`, Apache-2.0 License

//@compiler-options: target=es2020

const matches = /\w/g[Symbol.matchAll]("matchAll");
const array = [...matches];
const { index, input } = array[0];
