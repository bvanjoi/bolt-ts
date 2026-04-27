// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorOnUnionVsObjectShouldDeeplyDisambiguate2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Stuff {
  a?: () => Promise<number[]>;
  b: () => Promise<string>;
  c: () => Promise<string>;
  d: () => Promise<string>;
  e: () => Promise<string>;
  f: () => Promise<string>;
  g: () => Promise<string>;
  h: () => Promise<string>;
  i: () => Promise<string>;
  j: () => Promise<string>;
  k: () => Promise<number>;
}

function foo(): Stuff | Date {
  return {
    a() { return [123] }, //~ERROR: Type 'number[]' is missing the following properties from type 'Promise<number[]>': then, catch, and 1 more.
    b: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    c: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    d: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    e: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    f: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    g: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    h: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    i: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    j: () => "hello",     //~ERROR: Type 'string' is not assignable to type 'Promise<string>'.
    k: () => 123          //~ERROR: Type 'number' is not assignable to type 'Promise<number>'.
  }
}