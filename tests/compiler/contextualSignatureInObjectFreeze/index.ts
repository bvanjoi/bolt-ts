// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInObjectFreeze.ts`, Apache-2.0 License

//@compiler-options: target=es2020
//@compiler-options: lib=[es2019]

Object.freeze({
    f: function () { }
})
