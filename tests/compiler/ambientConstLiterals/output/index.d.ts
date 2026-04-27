declare function f<T>(x: T): T;
declare function g<T>(x: T): T;
declare enum E {
  A = 0,
  B = 1,
  C = 2,
  "non identifier" = 3
}
declare const c1: "abc"
declare const c2: 123
declare const c3: "abc"
declare const c4: 123
declare const c5: 123
declare const c6: -123
declare const c7: true
declare const c8: E.A
declare const c8b: E["non identifier"]
declare const c9: { x: string; }
declare const c10: number[]
declare const c11: string
declare const c12: number
declare const c13: "abc" | "def"
declare const c14: 123 | 456
