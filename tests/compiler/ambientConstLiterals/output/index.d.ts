function f<T>(x: T): T;
function g<T>(x: T): T;
enum E {
  A = 0,
  B = 1,
  C = 2,
  "non identifier" = 3
}
const c1: "abc";

const c2: 123;

const c3: "abc";

const c4: 123;

const c5: 123;

const c6: -123;

const c7: true;

const c8: E.A;

const c8b: E["non identifier"];

const c9: { x: string; };

const c10: number[];

const c11: string;

const c12: number;

const c13: "abc" | "def";

const c14: 123 | 456;

