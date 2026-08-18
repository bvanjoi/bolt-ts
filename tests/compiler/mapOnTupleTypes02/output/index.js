// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mapOnTupleTypes02.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: strictNullChecks
export function increment(point) {
  return point.map((d) => (d + 1));
}