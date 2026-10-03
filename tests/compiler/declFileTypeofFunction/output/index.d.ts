declare function f(n: typeof f): string;
declare function f(n: typeof g): string;

declare function g(n: typeof g): number;
declare function g(n: typeof f): number;

declare var b: () => typeof b;
declare function b1(): () => any;
declare function foo(): typeof foo;
declare var foo1: typeof foo;
declare var foo2: () => any;
declare var foo3: () => any;
declare var x: () => any;
declare function foo5(x: number): (x: number) => number;
