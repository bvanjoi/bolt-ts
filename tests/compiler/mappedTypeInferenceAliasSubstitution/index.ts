// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeInferenceAliasSubstitution.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

const v = { test: { smth: 5 } };

type Field<A extends string, R> = { [K in A]: R }

const f = <A extends string, B extends string, R>(x: { [K in A]: Field<B, R> }): R => ({} as any);
const r1 = f(v);

const g = <A extends string, B extends string, R>(x: Field<A, Field<B, R>>): R => ({} as any);
const r2 = g(v);
