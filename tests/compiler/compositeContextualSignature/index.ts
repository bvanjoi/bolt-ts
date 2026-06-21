// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/compositeContextualSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function f<T extends any[]>(v: ReadonlyArray<T>) { }

f([
    [
        undefined,
        () => { },
    ],
    [
        1,
        () => {
            console.log('Hello')
        },
    ],
]);
