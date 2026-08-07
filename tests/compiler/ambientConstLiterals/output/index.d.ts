function f<T>(x: T): T;
function g<T>(x: T): T;
enum E {
  A = 0,
  B = 1,
  C = 2,
  "non identifier" = 3
}
var c1: "abc";
var c2: 123;
var c3: "abc";
var c4: 123;
var c5: 123;
var c6: -123;
var c7: true;
var c8: E.A;
var c8b: E["non identifier"];
var c9: { x: string; };
var c10: number[];
var c11: string;
var c12: number;
var c13: "abc" | "def";
var c14: 123 | 456;
