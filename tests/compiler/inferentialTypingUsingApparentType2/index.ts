// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inferentialTypingUsingApparentType2.ts`, Apache-2.0 License

function foo<T extends { m(p: string): number }>(x: T): T {
  return undefined;
}

foo({ m(x) { return x.length } });
foo({ m(x) { 
  let y: number = x;
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  return x.length 
}})