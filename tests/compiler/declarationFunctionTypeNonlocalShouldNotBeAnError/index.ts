// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationFunctionTypeNonlocalShouldNotBeAnError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace foo {
    function bar(): void {}

    export const obj = {
        bar
    }
}
