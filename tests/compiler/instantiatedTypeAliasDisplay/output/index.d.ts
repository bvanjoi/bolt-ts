interface X<A> {
  a: A;
}
interface Y<B> {
  b: B;
}
type Z<A, B> = X<A> | Y<B>;
declare function f1<A>(): Z<A, number>;
declare function f2<A, B, C, D, E>(a: A, b: B, c: C, d: D): Z<A, string[]>;
declare const x1: Z<string, number>;

declare const x2: Z<{ }, string[]>;

