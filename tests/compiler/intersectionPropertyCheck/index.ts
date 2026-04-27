// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionPropertyCheck.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

let obj: { a: { x: string } } & { c: number } = { a: { x: 'hello', y: 2 }, c: 5 };  // Nested excess property
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type '{ x: string; }'.

declare let wrong: { a: { y: string } };
let weak: { a?: { x?: number } } & { c?: string } = wrong;  // Nested weak object type
//~^ ERROR: Type '{ a: { y: string; }; }' is not assignable to type '{ a: undefined | { x: undefined | number; }; } & { c: undefined | string; }'.

function foo<T extends object>(x: { a?: string }, y: T & { a: boolean }) {
  x = y;  // Mismatched property in source intersection
  //~^ ERROR: Type 'T & { a: boolean; }' is not assignable to type '{ a: undefined | string; }
}

// Repro from #36637

interface Test {
  readonly hi?: string[]
}

function test<T extends object>(value: T): Test {
  return { ...value, hi: true }
  //~^ ERROR: Type 'true' is not assignable to type 'undefined | string[]'.
}
