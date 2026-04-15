// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/mappedTypeOverArrayWithBareAnyRestCanBeUsedAsRestParam1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Replace<T extends [...any], A, B> = {
  [K in keyof T]: T[K] extends A ? B : T[K];
};

type ReplaceParams1<ARRAY extends [...any], A, B> = (
  ...args: Replace<ARRAY, A, B>
) => any;
