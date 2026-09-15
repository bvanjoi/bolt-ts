// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypeGeneratorReturnTypeFromUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: lib=[esnext]

type Action = () => (Generator<string, string, string[]> | string)

const test1: Action = function* () {
    const next = yield ''
    return next[0]
}

type Action2 = () => (AsyncGenerator<string, string, string[]> | string)

const test2: Action2 = async function* () {
    const next = yield await Promise.resolve('')
    return next[0]
}
