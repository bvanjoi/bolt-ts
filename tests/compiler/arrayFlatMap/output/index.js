// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayFlatMap.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: lib=[es2019]
var array = [];
var readonlyArray = [];
array.flatMap(() => ([]));// ok

readonlyArray.flatMap(() => ([]));