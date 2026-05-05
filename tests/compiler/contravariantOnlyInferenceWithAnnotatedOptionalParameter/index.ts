// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contravariantOnlyInferenceWithAnnotatedOptionalParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function filter<T>(predicate: (value: T, index: number) => boolean): T;
const a = filter((pose?: number) => true);
const b = filter((pose?: number, _?: number) => true);
