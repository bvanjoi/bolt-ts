// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/tupleTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var v1: [];  // Error
var v2: [number];
var v3: [number, string];
var v4: [number, [string, string]];

var t: [number, string];
var t0 = t[0];   // number
//~^ ERROR: Variable 't' is used before being assigned.
var t0: number;
var t1 = t[1];   // string
//~^ ERROR: Variable 't' is used before being assigned.
var t1: string;
var t2 = t[2];   // number|string
//~^ ERROR: Variable 't' is used before being assigned.
//~| ERROR: Tuple type '[number, string]' of length '2' has no element at index '2'.
var t2: number|string;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 't2' must be of type 'undefined', but here has type 'number | string'.

t = [];               // Error
//~^ ERROR: Type '[]' is not assignable to type '[number, string]'.
t = [1];              // Error
//~^ ERROR: Type '[number]' is not assignable to type '[number, string]'.
t = [1, "hello"];     // Ok
t = ["hello", 1];     // Error
//~^ ERROR: Type 'string' is not assignable to type 'number'.
//~| ERROR: Type 'number' is not assignable to type 'string'.
t = [1, "hello", 2];  // Error
//~^ ERROR: Type '[number, string, number]' is not assignable to type '[number, string]'.

var tf: [string, (x: string) => number] = ["hello", x => x.length];

declare function ff<T, U>(a: T, b: [T, (x: T) => U]): U;
var ff1 = ff("hello", ["foo", x => x.length]);
var ff1: number;

function tuple2<T0, T1>(item0: T0, item1: T1): [T0, T1]{
    return [item0, item1];
}

var tt = tuple2(1, "string");
var tt0 = tt[0];
var tt0: number;
var tt1 = tt[1];
var tt1: string;
var tt2 = tt[2];
//~^ ERROR: Tuple type '[number, string]' of length '2' has no element at index '2'.
var tt2: number | string;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'tt2' must be of type 'undefined', but here has type 'number | string'.

tt = tuple2(1, undefined);
//~^ ERROR: Type '[number, undefined]' is not assignable to type '[number, string]'.
tt = [1, undefined];  // Error
//~^ ERROR: Type 'undefined' is not assignable to type 'string'.
tt = [undefined, undefined];  // Error
//~^ ERROR: Type 'undefined' is not assignable to type 'number'.
//~| ERROR: Type 'undefined' is not assignable to type 'string'.
tt = [];  // Error
//~^ ERROR: Type '[]' is not assignable to type '[number, string]'.

var a: number[];
var a1: [number, string];
var a2: [number, number];
var a3: [number, {}];
a = a1;   // Error
//~^ ERROR: Variable 'a1' is used before being assigned.
//~| ERROR: Type '[number, string]' is not assignable to type 'number[]'.
a = a2;
//~^ ERROR: Variable 'a2' is used before being assigned.
a = a3;   // Error
//~^ ERROR: Variable 'a3' is used before being assigned.
//~| ERROR: Type '[number, { }]' is not assignable to type 'number[]'.
a1 = a2;  // Error
//~^ ERROR: Variable 'a2' is used before being assigned.
//~| ERROR: Type '[number, number]' is not assignable to type '[number, string]'.
a1 = a3;  // Error
//~^ ERROR: Variable 'a3' is used before being assigned.
//~| ERROR: Type '[number, { }]' is not assignable to type '[number, string]'.
a3 = a1;
a3 = a2;
//~^ ERROR: Variable 'a2' is used before being assigned.

type B = Pick<[number], 'length'>;
declare const b: B;
b.length = 0; // Error
//~^ ERROR: Type '0' is not assignable to type '1'.
declare const b1: readonly [number?];
b1.length = 0; // Error
//~^ ERROR: Cannot assign to 'length' because it is a read-only property.
declare const b2: readonly [number, ...number[]];
b2.length = 0; // Error
//~^ ERROR: Cannot assign to 'length' because it is a read-only property.
declare const b3: readonly number[];
b3.length = 0; // Error
//~^ ERROR: Cannot assign to 'length' because it is a read-only property.
declare const b4: [number?];
b4.length = 0;
