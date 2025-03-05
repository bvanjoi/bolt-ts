// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/excessPropertyCheckingIntersectionWithConditional.ts`, Apache-2.0 License

type Foo<K> = K extends unknown ? { a: number } : unknown
const createDefaultExample = <K,>(x: K): Foo<K> & { x: K; } => {
  return { a: 1, x: x }; // okay in TS 4.7.4, error in TS 4.8.2
}

const foo = <K,>(x: K): Foo<K> & { x: K; } => {
  return { a: '1', x: x };
  //~^ ERROR: Type '{ a: string; x: K; }' is not assignable to type 'cond & { x: K; }'.
}