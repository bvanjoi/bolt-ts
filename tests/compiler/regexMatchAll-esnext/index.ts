// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/regexMatchAll-esnext.ts`, Apache-2.0 License

//@compiler-options: target=esnext

const matches = /\w/g[Symbol.matchAll]("matchAll");
const array = [...matches];
const { index, input } = array[0];
