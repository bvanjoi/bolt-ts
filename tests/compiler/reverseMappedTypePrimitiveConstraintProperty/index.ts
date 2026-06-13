// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedTypePrimitiveConstraintProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function test<
  T extends { prop: string; nested: { nestedProp: string } },
>(obj: { [K in keyof T]: T[K] }): T;

const result = test({
  prop: "foo", // this one should not widen to string
  nested: {
    nestedProp: "bar",
  },
  extra: "baz",
});