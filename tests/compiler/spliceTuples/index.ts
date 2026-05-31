// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spliceTuples.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare const sb: [string, boolean];

let k1: [number, string, boolean];
k1 = [1, ...sb];

let k2: [number, string, boolean, number];
k2 = [1, ...sb, 1];

declare const sb_: [string, ...boolean[]];

let k3: [number, string, ...boolean[]];
k3 = [1, ...sb_];

declare const sbb_: [string, boolean, ...boolean[]];

let k4: [number, string, ...boolean[]];
k4 = [1, ...sbb_];

let k5: [number, string, boolean, ...boolean[]];
k5 = [1, ...sbb_];

let k6: [number, string, boolean, boolean, ...boolean[]];
k6 = [1, ...sbb_];
//~^ ERROR: Type '[number, string, boolean, ...boolean[]]' is not assignable to type '[number, string, boolean, boolean, ...boolean[]]'.
