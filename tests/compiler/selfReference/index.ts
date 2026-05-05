// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/selfReference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
//@run-fail

declare function asFunction<T>(value: T): () => T;
asFunction(() => { return 1; });