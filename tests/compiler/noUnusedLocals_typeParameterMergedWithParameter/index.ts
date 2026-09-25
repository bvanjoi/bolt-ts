// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_typeParameterMergedWithParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function useNone<T>(T: number) {}
//~^ ERROR: 'T' is declared but its value is never read.
//~| ERROR: 'T' is declared but its value is never read.

function useParam<T>(T: number) {
//~^ ERROR: 'T' is declared but its value is never read.
    return T;
}

function useTypeParam<T>(T: T) {}
//~^ ERROR: 'T' is declared but its value is never read.

function useBoth<T>(T: T) {
    return T;
}
