type FibWithCache<N extends number, A extends number[] = [], B extends number[] = [1], Count extends number[] = []> =
  Count['length'] extends N
  ? A['length']
  : FibWithCache<N, B, [...A, ...B], [1, ...Count]>;

let e0: FibWithCache<0> = 1;  //~ERROR: Type '1' is not assignable to type '0'.
let e1: FibWithCache<1> = 2;  //~ERROR: Type '2' is not assignable to type '1'.
let e2: FibWithCache<2> = 2;  //~ERROR: Type '2' is not assignable to type '1'. 
let e3: FibWithCache<3> = 3;  //~ERROR: Type '3' is not assignable to type '2'. 
let e4: FibWithCache<4> = 4;  //~ERROR: Type '4' is not assignable to type '3'. 
let e5: FibWithCache<5> = 6;  //~ERROR: Type '6' is not assignable to type '5'. 
let e6: FibWithCache<6> = 9;  //~ERROR: Type '9' is not assignable to type '8'. 
let e7: FibWithCache<7> = 14; //~ERROR: Type '14' is not assignable to type '13'. 
let f18: FibWithCache<18> = 2584;
let e19: FibWithCache<19> = 4182; //~ERROR: Type '4182' is not assignable to type '4181'. 
let f19: FibWithCache<19> = 4181;
let f20: FibWithCache<20> = 6765;

