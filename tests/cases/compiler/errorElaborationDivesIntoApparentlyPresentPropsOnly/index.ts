// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/errorElaborationDivesIntoApparentlyPresentPropsOnly.ts`, Apache-2.0 License

function foo<T extends { a: string }>(x: T) {
  x = { a: "abc", b: 20, c: 30 };
  //~^ ERROR: Type '{ b: number; a: string; c: number; }' is not assignable to type 'T'.
}

function bar<T extends { a: string }>(x: T) {
  x = { a: 20 };
  //~^ ERROR: Type '{ a: number; }' is not assignable to type 'T'.
}

function baz<T extends { a: string }>(x: T) {
  x = { a: "not ok" };
  //~^ ERROR: Type '{ a: string; }' is not assignable to type 'T'.
}
