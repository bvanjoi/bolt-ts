// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTypeAliasWithTypeParameters1.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration

export type Bar<X, Y> = () => [X, Y];
export type Foo<Y> = Bar<any, Y>;
export const y = (x: Foo<string>) => 1
