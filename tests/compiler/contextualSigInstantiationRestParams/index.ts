// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSigInstantiationRestParams.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare function toInstantiate<A, B>(a?: A, b?: B): B;
declare function contextual(...s: string[]): string

var sig: typeof contextual = toInstantiate;