type Fib<N extends number, A extends number[] = [], B extends number[] = [1], Count extends number[] = []> =
  Count['length'] extends N
  ? A['length']
  : Fib<N, B, [...A, ...B], [1, ...Count]>;

type F0 = Fib<0>; // 0
type F1 = Fib<1>; // 1
type F2 = Fib<2>; // 1
type F3 = Fib<3>; // 2
type F4 = Fib<4>; // 3
type F5 = Fib<5>; // 5
type F6 = Fib<6>; // 8
type F7 = Fib<7>; // 13
type F19 = Fib<19> // 4181

let f0: F0 = 0;
let f1: F1 = 1;
let f2: F2 = 1;
let f3: F3 = 2;
let f4: F4 = 3;
let f5: F5 = 5;
let f6: F6 = 8;
let f7: F7 = 13;
let f19: F19 = 4181;