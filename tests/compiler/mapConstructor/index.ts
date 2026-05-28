// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mapConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

new Map();

const potentiallyUndefinedIterable = [['1', 1], ['2', 2]] as Iterable<[string, number]> | undefined;
new Map(potentiallyUndefinedIterable);

const potentiallyNullIterable = [['1', 1], ['2', 2]] as Iterable<[string, number]> | null;
new Map(potentiallyNullIterable);