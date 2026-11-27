// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeGenericInstantiationPreservesInlineForm.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@compiler-options: emitDeclarationOnly

export const test1 = <T = Record<string, never>>(schema: {
    [K in keyof Required<T>]: T[K];
}) => {}

export function test2<T = Record<string, never>>(schema: {
    [K in keyof Required<T>]: T[K];
}) {};

