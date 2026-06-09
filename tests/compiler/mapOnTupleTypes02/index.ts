// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mapOnTupleTypes02.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: strictNullChecks
//@compiler-options: noImplicitAny

export type Point = [number, number];

export function increment(point: Point) {
  return point.map(d => d + 1);
}
