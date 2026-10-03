// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayFlatMap.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es2019]

const array: number[] = [];
const readonlyArray: ReadonlyArray<number> = [];
array.flatMap((): ReadonlyArray<number> => []); // ok
readonlyArray.flatMap((): ReadonlyArray<number> => []); // ok
