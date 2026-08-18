// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTypeAliasWithTypeParameters2.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration

export type Bar<X, Y, Z> = () => [X, Y, Z];
export type Baz<M, N> = Bar<M, string, N>;
export type Baa<Y> = Baz<boolean, Y>;
export const y = (x: Baa<number>) => 1