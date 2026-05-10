// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionRelationshipCheckPasses.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const item: { foo?: undefined } | { foo: number } = null as any as { foo?: number | undefined };