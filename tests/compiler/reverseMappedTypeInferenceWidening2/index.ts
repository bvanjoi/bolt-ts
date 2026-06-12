// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedTypeInferenceWidening2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function test1<T extends Record<string, { prop: unknown }>>(arg: {
  [K in keyof T]: T[K];
}): T;

const res1 = test1({
  foo: {
    prop: 1,
    prop2: "",
  },
  bar: {
    prop: true,
    prop2: null,
  },
});
