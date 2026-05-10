// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/partiallyAmbientFundule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace foo {
    export function x(): any;
}
function foo () { } // Legal, because module is ambient