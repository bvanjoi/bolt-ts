// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitLambdaWithMissingTypeParameterNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

export interface Foo {
    preFetch: <T1 extends T2> (c: T1) => void; // Type T2 is not defined
    //~^ ERROR: Cannot find name 'T2'.
    preFetcher: new <T1 extends T2> (c: T1) => void; // Type T2 is not defined
    //~^ ERROR: Cannot find name 'T2'.
}