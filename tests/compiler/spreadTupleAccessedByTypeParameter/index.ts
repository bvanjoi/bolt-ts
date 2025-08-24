// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/spreadTupleAccessedByTypeParameter.ts`, Apache-2.0 License

export function test<N extends number>(singletons: ["a"][], i: N) {
  const singleton = singletons[i];
  const [, ...rest] = singleton;

  return rest;
}
