interface A {
  a: string;
}
interface B extends A {
  b: string;
}
interface C extends B {
  c: string;
}
interface G<T, U extends B> {
  x: T;
  y: U;
}
declare var v1: G<A, C>;

declare var v2: G<{
  a: string;
}, C>;

declare var v4: G<G<A, B>, C>;

