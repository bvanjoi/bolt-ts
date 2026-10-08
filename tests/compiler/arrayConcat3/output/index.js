// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayConcat3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictFunctionTypes
function doStuff(a, b) {
  b.concat(a);
}