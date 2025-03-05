// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inferentialTypingUsingApparentType1.ts`, Apache-2.0 License

function foo<T extends (p: string) => number>(x: T): T {
  return undefined;
}

foo(x => x.length);
foo(x => {
  let y: number = x;
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  return x;
});