// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypesNegatedTypeLikeConstraintInGenericMappedType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Tags<D extends string, P> = P extends Record<D, infer X> ? X : never;

declare const typeTags: <I>() => <
  P extends {
    readonly [Tag in Tags<"_tag", I> & string]: (
      _: Extract<I, { readonly _tag: Tag }>,
    ) => any;
  } & { readonly [Tag in Exclude<keyof P, Tags<"_tag", I>>]: never },
>(
  fields: P,
) => unknown;

type Value = { _tag: "A"; a: number } | { _tag: "B"; b: number };
const matcher = typeTags<Value>();

matcher({
  A: (_) => _.a,
  B: (_) => "fail",
});

matcher({
  A: (_) => _.a,
  B: (_) => "fail",
  C: (_) => "fail",
  //~^ ERROR: Type '(_: any) => string' is not assignable to type 'never'.
  //~| ERROR: Parameter '_' implicitly has an 'any' type.
});
