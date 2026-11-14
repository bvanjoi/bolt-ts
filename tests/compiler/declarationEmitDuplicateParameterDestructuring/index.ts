// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDuplicateParameterDestructuring.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: emitDeclarationOnly

export const fn1 = ({ prop: a, prop: b }: { prop: number }) => a + b;

export const fn2 = ({ prop: a }: { prop: number }, { prop: b }: { prop: number }) => a + b;
