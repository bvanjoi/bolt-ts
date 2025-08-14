// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/indexedAccessToThisTypeOnIntersection01.ts`, Apache-2.0 License

interface A {
  a: string;
  getA: this['a'];
}

type T = (A & { a: number })['getA'];

let a0: T = 'a';
//~^ ERROR: Type 'string' is not assignable to type 'never'.
let a1: A['getA'] = '42';