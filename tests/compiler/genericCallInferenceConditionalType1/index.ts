// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallInferenceConditionalType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/59108

declare const f: <T>(f: (x: T) => unknown) => (x: T) => unknown;
declare const g: <T extends unknown>(x: { foo: T }) => unknown;

const h = f(g);

type FirstParameter<T> = T extends (x: infer P) => unknown ? P : unknown;

type X = FirstParameter<typeof h>["foo"];
