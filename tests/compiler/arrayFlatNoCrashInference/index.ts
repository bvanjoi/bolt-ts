// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayFlatNoCrashInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: lib=[es2020]

function foo<T>(arr: T[], depth: number) {
    return arr.flat(depth);
}
