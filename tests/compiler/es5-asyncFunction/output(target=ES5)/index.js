
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es5-asyncFunction.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: lib=[es5,es2015.promise]
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
async function empty() {}
async function singleAwait() {
  await x;
}