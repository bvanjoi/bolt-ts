// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forAwaitForUnion.ts`, Apache-2.0 License
//@compiler-options: target=es2018
//@compiler-options: lib=[esnext]
async function f(source) {
  for await ( var x of source) {}
}