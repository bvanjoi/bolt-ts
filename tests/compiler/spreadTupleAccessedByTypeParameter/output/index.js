// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/spreadTupleAccessedByTypeParameter.ts`, Apache-2.0 License
export function test(singletons, i) {
  var singleton = singletons[i];
  var [, ...rest] = singleton;
  return rest;
}