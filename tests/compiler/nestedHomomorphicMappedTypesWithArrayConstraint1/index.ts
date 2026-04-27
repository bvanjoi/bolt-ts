// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedHomomorphicMappedTypesWithArrayConstraint1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type MatchArguments<T> = {
    [K in keyof T]: T[K];
};

interface SinonSpyCallApi<TArgs extends any[] = any[]> {
    calledWith(...args: Partial<MatchArguments<TArgs>>): boolean;
}