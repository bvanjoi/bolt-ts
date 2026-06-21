// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/comparisonOfPartialDeepAndIndexedAccessTerminatesWithoutError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=true

type PartialDeep<T> = {[K in keyof T]?: PartialDeep<T[K]>};
type Many<T> = T | readonly T[];

interface Collection<T> {
    sortBy(...iteratees: Many<PartialDeep<T>>[]): Collection<T>;
}

const x: Collection<{x: number}> = (null as any as Collection<{x: number, y: number}>);

export {};
