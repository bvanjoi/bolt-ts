// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/indexedAccessToThisTypeOnIntersection01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface A {
  a: string;
  getA: this['a'];
}

type T = (A & { a: number })['getA'];

let a0: T = 'a';
//~^ ERROR: Type 'string' is not assignable to type 'never'.
let a1: A['getA'] = '42';

{
  type T0<T1> = keyof T1;
  type T2<T3 extends (boolean | any[])> = T0<number & T3>;
}