// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mapConstructor.ts`, Apache-2.0 License
//@compiler-options: target=es2015
new Map();
var potentiallyUndefinedIterable = [['1', 1], ['2', 2]];
new Map(potentiallyUndefinedIterable);
var potentiallyNullIterable = [['1', 1], ['2', 2]];
new Map(potentiallyNullIterable);